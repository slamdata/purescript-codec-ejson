module Data.Codec.EJson.Compat
  ( module Data.Codec.EJson.Compat
  , module Data.Codec.EJson
  , module Common
  ) where

import Prelude hiding (map)

import Data.Codec (basicCodec)
import Data.Codec.EJson (EJIndexedCodec, EJPropCodec, EJsonCodec, EJsonDecodeError(..), array, boolean, decimal, decode, ejarray, ejmap, ejson, encode, index, indexedArray, int, map, null, printEJsonDecodeError, prop, prop', string, (<~<), (~))
import Data.Codec.EJson.Common (either, list, map, tuple, strMap) as Common
import Data.Json.Extended as EJ
import Data.Bifunctor as BF
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Lens (is)

-- | A codec for `Maybe` values.
-- |
-- | Note: this codec cannot represent nested `Maybe` values in a lossless
-- | manner.
maybe ∷ ∀ a. EJsonCodec a → EJsonCodec (Maybe a)
maybe codec = basicCodec dec enc
  where
  dec ∷ EJ.EJson → Either EJsonDecodeError (Maybe a)
  dec j
    | is EJ._Null j = pure Nothing
    | otherwise = BF.bimap (Named "Maybe") Just ((decode codec j))
  enc ∷ Maybe a → EJ.EJson
  enc = case _ of
    Nothing → EJ.null
    Just a → encode codec a
