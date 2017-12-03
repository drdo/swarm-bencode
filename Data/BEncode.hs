{-# LANGUAGE UnicodeSyntax #-}

module Data.BEncode
  ( module Data.BEncode.Types
  , decode
  , eitherDecode
  , encode
  )
where

import Control.Monad ((>=>))
import qualified Data.Attoparsec.ByteString.Lazy as AP (eitherResult, parse)
import qualified Data.BEncode.Builder as BEBuilder (value)
import qualified Data.BEncode.Parser as BEParser (value)
import Data.BEncode.Types
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)

--------------------------------------------------------------------------------
decode ∷ FromBEncode α ⇒ LBS.ByteString → Maybe α
decode = either (const Nothing) Just . eitherDecode

eitherDecode ∷ FromBEncode α ⇒ LBS.ByteString → Either String α
eitherDecode = (AP.eitherResult . AP.parse BEParser.value)
           >=> (parse . parseBEncode)

encode ∷ ToBEncode α ⇒ α → LBS.ByteString
encode = toLazyByteString . BEBuilder.value . toBEncode
