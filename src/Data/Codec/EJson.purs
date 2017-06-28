module Data.Codec.EJson
  ( EJsonCodec
  , EJsonDecodeError(..)
  , printEJsonDecodeError
  , ejson
  , null
  , boolean
  , decimal
  , int
  , string
  , char
  , ejarray
  , ejmap
  , array
  , value
  , EJIndexedCodec
  , indexedArray
  , index
  , EJPropCodec
  , map
  , prop
  , prop'
  , optionalProp
  , optionalProp'
  , module Exports
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (writer, mapWriter)
import Data.Array as A
import Data.Bifunctor as BF
import Data.Codec (BasicCodec, Codec, GCodec(..), basicCodec, bihoistGCodec, decode, encode, (<~<))
import Data.Codec (decode, encode, (~), (<~<)) as Exports
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Json.Extended as EJ
import Data.Json.Extended.Type as EJT
import Data.Lens (Prism', preview, review)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Star (Star(..))
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | Codec type for `Json` values.
type EJsonCodec a = BasicCodec (Either EJsonDecodeError) EJ.EJson a

-- | Error type for failures while decoding.
data EJsonDecodeError
  = TypeMismatch EJT.EJsonType
  | UnexpectedValue EJ.EJson
  | AtIndex Int EJsonDecodeError
  | AtKey EJ.EJson EJsonDecodeError
  | Named String EJsonDecodeError
  | MissingValue

derive instance eqEJsonDecodeError ∷ Eq EJsonDecodeError
derive instance ordEJsonDecodeError ∷ Ord EJsonDecodeError
derive instance genericEJsonDecodeError ∷ Generic EJsonDecodeError _

instance showEJsonDecodeError ∷ Show EJsonDecodeError where
  show err = genericShow err

printEJsonDecodeError ∷ EJsonDecodeError → String
printEJsonDecodeError err =
  "An error occurred while decoding a EJSON value:\n" <> go err
  where
    go = case _ of
      TypeMismatch ty → "  Expected value of type '" <> show ty <> "'."
      UnexpectedValue val → "  Unexpected value '" <> EJ.renderEJson val <> "'."
      AtIndex ix inner → "  At array index " <> show ix <> ":\n" <> go inner
      AtKey key inner → "  At map key " <> EJ.renderEJson key <> ":\n" <> go inner
      Named name inner → "  Under '" <> name <> "':\n" <> go inner
      MissingValue → "  No value was found."

ejson ∷ EJsonCodec EJ.EJson
ejson = basicCodec pure id

null ∷ EJsonCodec Unit
null = prismTypeCodec EJT.Null EJ._Null

boolean ∷ EJsonCodec Boolean
boolean = prismTypeCodec EJT.Boolean EJ._Boolean

decimal ∷ EJsonCodec HN.HugeNum
decimal = prismTypeCodec EJT.Decimal EJ._Decimal

number ∷ EJsonCodec (Either HI.HugeInt HN.HugeNum)
number = prismTypeCodec EJT.Decimal EJ._Number

int ∷ EJsonCodec HI.HugeInt
int = prismTypeCodec EJT.Integer EJ._Integer

string ∷ EJsonCodec String
string = prismTypeCodec EJT.String EJ._String

char ∷ EJsonCodec Char
char = cc <~< prismTypeCodec EJT.String EJ._String
  where
  cc ∷ BasicCodec (Either EJsonDecodeError) String Char
  cc = basicCodec (\s → note (Named "Char" (UnexpectedValue (EJ.string s))) $ Str.toChar s) Str.singleton

ejarray ∷ EJsonCodec (Array EJ.EJson)
ejarray = prismTypeCodec EJT.Array EJ._Array

ejmap ∷ EJsonCodec (M.Map EJ.EJson EJ.EJson)
ejmap = prismTypeCodec EJT.Map EJ._Map

value ∷ EJ.EJson → EJsonCodec EJ.EJson
value x = basicCodec dec (pure x)
  where
  dec ∷ EJ.EJson → Either EJsonDecodeError EJ.EJson
  dec x'
    | x == x' = Right x
    | otherwise = Left (UnexpectedValue x')

array ∷ ∀ a. EJsonCodec a → EJsonCodec (Array a)
array codec = GCodec dec enc
  where
  dec = ReaderT \j →
    traverse (\(Tuple ix j') → BF.lmap (AtIndex ix) (decode codec j'))
      <<< A.mapWithIndex Tuple
      =<< decode ejarray j
  enc = Star \xs → writer $ Tuple xs (EJ.array (encode codec <$> xs))

type EJIndexedCodec a =
  Codec
    (Either EJsonDecodeError)
    (Array EJ.EJson)
    (L.List EJ.EJson)
    a a

indexedArray ∷ ∀ a. String → EJIndexedCodec a → EJsonCodec a
indexedArray name =
  bihoistGCodec
    (\r → ReaderT (BF.lmap (Named name) <<< runReaderT r <=< decode ejarray))
    (mapWriter (BF.rmap (EJ.array <<< A.fromFoldable)))

index ∷ ∀ a. Int → EJsonCodec a → EJIndexedCodec a
index ix codec = GCodec dec enc
  where
  dec = ReaderT \xs →
    BF.lmap (AtIndex ix) case A.index xs ix of
      Just val → decode codec val
      Nothing → Left MissingValue
  enc = Star \val → writer $ Tuple val (pure (encode codec val))

type EJPropCodec a =
  Codec
    (Either EJsonDecodeError)
    (M.Map EJ.EJson EJ.EJson)
    (L.List (Tuple EJ.EJson EJ.EJson))
    a a

map ∷ ∀ a. String → EJPropCodec a → EJsonCodec a
map name =
  bihoistGCodec
    (\r → ReaderT (BF.lmap (Named name) <<< runReaderT r <=< decode ejmap))
    (mapWriter (BF.rmap (EJ.map <<< M.fromFoldable)))

prop ∷ ∀ a. EJ.EJson → EJsonCodec a → EJPropCodec a
prop key codec = GCodec dec enc
  where
  dec ∷ ReaderT (M.Map EJ.EJson EJ.EJson) (Either EJsonDecodeError) a
  dec = ReaderT \obj →
    BF.lmap (AtKey key) case M.lookup key obj of
      Just val → decode codec val
      Nothing → Left MissingValue
  enc = Star \val → writer $ Tuple val (pure (Tuple key (encode codec val)))

prop' ∷ ∀ a. String → EJsonCodec a → EJPropCodec a
prop' = prop <<< EJ.string

optionalProp ∷ ∀ a. EJ.EJson → EJsonCodec a → EJPropCodec (Maybe a)
optionalProp key codec = GCodec dec enc
  where
  dec ∷ ReaderT (M.Map EJ.EJson EJ.EJson) (Either EJsonDecodeError) (Maybe a)
  dec = ReaderT \obj →
    BF.lmap (AtKey key) case M.lookup key obj of
      Just val → Just <$> decode codec val
      Nothing → pure Nothing
  enc = Star \v → case v of
    Just val → writer $ Tuple v (pure (Tuple key (encode codec val)))
    Nothing → writer $ Tuple v L.Nil

optionalProp' ∷ ∀ a. String → EJsonCodec a → EJPropCodec (Maybe a)
optionalProp' = optionalProp <<< EJ.string

prismTypeCodec
  ∷ ∀ a
   . EJT.EJsonType
  → Prism' EJ.EJson a
  → EJsonCodec a
prismTypeCodec ty prism =
  basicCodec
    (maybe (Left (TypeMismatch ty)) pure <<< preview prism)
    (review prism)
