{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.BEncode.Builder
  ( string
  , value
  )
where

import Data.BEncode.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Builder (Builder, byteString, integerDec, intDec)
import qualified Data.Map as Map (foldMapWithKey)
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
string ∷ ByteString → Builder
string s = intDec (BS.length s) <> ":" <> byteString s

value ∷ Value → Builder
value = \case
  String s → string s
  Integer i → "i" <> integerDec i <> "e"
  List l → "l" <> foldMap value l <> "e"
  Dict d → "d" <> Map.foldMapWithKey (\k v → string k <> value v) d <> "e"
