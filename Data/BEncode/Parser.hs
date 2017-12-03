{-# LANGUAGE UnicodeSyntax #-}

module Data.BEncode.Parser
  ( string
  , value
  )
where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as AP
  (Parser, char, decimal, take)
import qualified Data.Attoparsec.Combinator as AP (manyTill)
import Data.BEncode.Types
import Data.ByteString (ByteString)
import qualified Data.Map as Map (fromList)

--------------------------------------------------------------------------------
string ∷ AP.Parser ByteString
string = AP.decimal <* AP.char ':' >>= AP.take

value ∷ AP.Parser Value
value =  Integer <$> (AP.char 'i' *> AP.decimal <* AP.char 'e')
     <|> List <$> (AP.char 'l' *> AP.manyTill value (AP.char 'e'))
     <|> Dict . Map.fromList
         <$> (AP.char 'd' *> AP.manyTill ((,) <$> string <*> value) (AP.char 'e'))
     <|> String <$> string
