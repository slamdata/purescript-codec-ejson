module Data.Codec.EJson.Sum
  ( Tag(..)
  , taggedSum
  ) where

import Prelude

import Data.Codec (GCodec(..), decode, encode)
import Data.Codec.EJson (EJsonCodec, EJsonDecodeError, ejmap, ejson, prop', string)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Writer (Writer, writer)
import Data.Either as E
import Data.Json.Extended as EJ
import Data.Map as M
import Data.Newtype (class Newtype)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))

newtype Tag = Tag String

derive newtype instance eqTag ∷ Eq Tag
derive newtype instance ordTag ∷ Ord Tag
derive instance newtypeTag ∷ Newtype Tag _

taggedSum
  ∷ ∀ a
  . (Tag → EJ.EJson → E.Either EJsonDecodeError a)
  → (a → Tuple Tag EJ.EJson)
  → EJsonCodec a
taggedSum f g = GCodec (decodeCase f) (encodeCase g)

decodeCase
  ∷ ∀ a
  . (Tag → EJ.EJson → E.Either EJsonDecodeError a)
  → ReaderT EJ.EJson (E.Either EJsonDecodeError) a
decodeCase f = ReaderT \j → do
  obj ← decode ejmap j
  tag ← decode (prop' "tag" string) obj
  value ← decode (prop' "value" ejson) obj
  f (Tag tag) value

encodeCase
  ∷ ∀ a
  . (a → Tuple Tag EJ.EJson)
  → Star (Writer EJ.EJson) a a
encodeCase f = Star case _ of
  a | Tuple (Tag tag) value ← f a →
    writer $ Tuple a $ encode ejmap $
      M.fromFoldable
        [ Tuple (EJ.string "tag") (encode string tag)
        , Tuple (EJ.string "value") value
        ]
