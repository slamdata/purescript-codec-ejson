module Data.Codec.EJson.Common
  ( module Data.Codec.EJson.Common
  , module Data.Codec.EJson
  ) where

import Prelude hiding (map)

import Data.Codec (basicCodec, mapCodec)
import Data.Codec.EJson (EJIndexedCodec, EJPropCodec, EJsonCodec, EJsonDecodeError(..), array, boolean, decimal, decode, ejarray, ejmap, ejson, encode, index, indexedArray, int, map, null, printEJsonDecodeError, prop, prop', string, (<~<), (~))
import Data.Codec.EJson.Sum (Tag(..), taggedSum)
import Data.Json.Extended as EJ
import Data.Array as A
import Data.Bifunctor as BF
import Data.Either (Either(..))
import Data.List as L
import Data.Functor as F
import Data.Traversable (traverse)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), fst, snd)

maybe ∷ ∀ a. EJsonCodec a → EJsonCodec (Maybe a)
maybe codec = basicCodec dec enc
  where
  dec j = do
    obj ← decode ejmap j
    tag ← decode (prop' "tag" string) obj
    case tag of
      "Just" → Just <$> decode (prop' "value" codec) obj
      "Nothing" → pure Nothing
      _ → Left (AtKey (EJ.string "tag") (UnexpectedValue (EJ.string tag)))
  enc x = encode ejmap $ case x of
    Nothing →
      M.singleton (EJ.string "tag") (EJ.string "Nothing")
    Just a → do
      M.fromFoldable
        [ Tuple (EJ.string "tag") (EJ.string "Just")
        , Tuple (EJ.string "value") (encode codec a)
        ]

tuple ∷ ∀ a b. EJsonCodec a → EJsonCodec b → EJsonCodec (Tuple a b)
tuple codecA codecB = indexedArray "Tuple" $
  Tuple
    <$> fst ~ index 0 codecA
    <*> snd ~ index 1 codecB

either ∷ ∀ a b. EJsonCodec a → EJsonCodec b → EJsonCodec (Either a b)
either codecA codecB = taggedSum dec enc
  where
  dec tag json = case tag of
    Tag "Left" → BF.bimap (AtKey (EJ.string "value")) Left (decode codecA json)
    Tag "Right" → BF.bimap (AtKey (EJ.string "value")) Right (decode codecB json)
    Tag t → Left (AtKey (EJ.string "tag") (UnexpectedValue (EJ.string t)))
  enc = case _ of
    Left a → Tuple (Tag "Left") (encode codecA a)
    Right b → Tuple (Tag "Right") (encode codecB b)

list ∷ ∀ a. EJsonCodec a → EJsonCodec (L.List a)
list = dimap A.fromFoldable L.fromFoldable <<< array

strMap ∷ ∀ a. EJsonCodec a → EJsonCodec (SM.StrMap a)
strMap codec =
  mapCodec
    (BF.lmap (Named "StrMap") <<< F.map fromArray <<< traverse decodeItem <<< M.toUnfoldable)
    (M.fromFoldable <<< F.map (BF.bimap EJ.string (encode codec)) <<< toArray)
    ejmap
  where
  fromArray ∷ ∀ v. Array (Tuple String v) → SM.StrMap v
  fromArray = SM.fromFoldable
  toArray ∷ ∀ v. SM.StrMap v → Array (Tuple String v)
  toArray = SM.toUnfoldable
  decodeItem ∷ Tuple EJ.EJson EJ.EJson → Either EJsonDecodeError (Tuple String a)
  decodeItem (Tuple key value) = do
    sk ← decode string key
    BF.bimap (AtKey key) (Tuple sk) (decode codec value)
