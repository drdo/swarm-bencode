{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.BEncode.Types
  ( Value (..)
  , Dict
  , Parser
  , parse
  , FromBEncode (..)
  , (.:)
  , (.:?)
  , (.!=)
  , ToBEncode (..)
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BSU8
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Word

--------------------------------------------------------------------------------
data Value
  = String ByteString
  | Integer Integer
  | List [Value]
  | Dict Dict
  deriving
    (Eq, Show)

type Dict = Map ByteString Value

--------------------------------------------------------------------------------
data Parser α
  = Parser
      { runParser ∷ ∀r. (String → r) → (α → r) → r
      }

instance Functor Parser where
  fmap g p = Parser $ \fK sK → runParser p fK (sK . g)

instance Applicative Parser where
  pure x = Parser $ \_ sK → sK x
  pG <*> pX = Parser $ \fK sK → runParser pG fK (\g → runParser pX fK (sK . g))

instance Monad Parser where
  return = pure
  pX >>= gP = Parser $ \fK sK → runParser pX fK (\x → runParser (gP x) fK sK)
  fail s = Parser $ \fK _ → fK s

instance Alternative Parser where
  empty = fail "empty"
  pX <|> pY = Parser $ \fK sK → runParser pX (const $ runParser pY fK sK) sK

instance MonadPlus Parser where
  mzero = fail "mzero"
  mplus = (<|>)

parse ∷ Parser α → Either String α
parse p = runParser p Left Right

--------------------------------------------------------------------------------
class FromBEncode α where
  parseBEncode ∷ Value → Parser α

instance FromBEncode Value where
  parseBEncode = pure

instance FromBEncode ByteString where
  parseBEncode (String s) = Parser $ \_ sK → sK s
  parseBEncode _ = fail "Expected a String"

instance {-# OVERLAPPING #-} FromBEncode [Char] where
  parseBEncode = fmap BSU8.toString . parseBEncode

instance FromBEncode Int where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Int8 where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Int16 where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Int32 where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Int64 where
  parseBEncode = fmap fromInteger . parseBEncode

instance FromBEncode Word where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Word8 where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Word16 where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Word32 where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Word64 where
  parseBEncode = fmap fromInteger . parseBEncode

instance FromBEncode Integer where
  parseBEncode (Integer i) = Parser $ \_ sK → sK i
  parseBEncode _ = fail "Expected an Integer"

instance FromBEncode Float where
  parseBEncode = fmap fromInteger . parseBEncode
instance FromBEncode Double where
  parseBEncode = fmap fromInteger . parseBEncode

instance FromBEncode α ⇒ FromBEncode [α] where
  parseBEncode (List vs) = traverse parseBEncode vs
  parseBEncode _ = fail "Expected a List"

instance FromBEncode α ⇒ FromBEncode (Map ByteString α) where
  parseBEncode (Dict d) = traverse parseBEncode d

(.:) ∷ FromBEncode α ⇒ Dict → ByteString → Parser α
d .: x = maybe (fail $ "Missing key " ++ show x) parseBEncode $ Map.lookup x d

(.:?) ∷ FromBEncode α ⇒ Dict → ByteString → Parser (Maybe α)
d .:? x = maybe (pure Nothing) (fmap Just . parseBEncode) $ Map.lookup x d

(.!=) ∷ Parser (Maybe α) → α → Parser α
(.!=) p x = p >>= maybe (pure x) pure

--------------------------------------------------------------------------------
class ToBEncode α where
  toBEncode ∷ α → Value

instance ToBEncode Value where
  toBEncode = id

instance ToBEncode ByteString where
  toBEncode = String

instance {-# OVERLAPPING #-} ToBEncode [Char] where
  toBEncode = String . BSU8.fromString

instance ToBEncode Int where
  toBEncode = Integer . fromIntegral
instance ToBEncode Int8 where
  toBEncode = Integer . fromIntegral
instance ToBEncode Int16 where
  toBEncode = Integer . fromIntegral
instance ToBEncode Int32 where
  toBEncode = Integer . fromIntegral
instance ToBEncode Int64 where
  toBEncode = Integer . fromIntegral

instance ToBEncode Word where
  toBEncode = Integer . fromIntegral
instance ToBEncode Word8 where
  toBEncode = Integer . fromIntegral
instance ToBEncode Word16 where
  toBEncode = Integer . fromIntegral
instance ToBEncode Word32 where
  toBEncode = Integer . fromIntegral
instance ToBEncode Word64 where
  toBEncode = Integer . fromIntegral

instance ToBEncode Integer where
  toBEncode = Integer

instance ToBEncode α ⇒ ToBEncode [α] where
  toBEncode = List . map toBEncode

instance ToBEncode α ⇒ ToBEncode (Map ByteString α) where
  toBEncode = Dict . fmap toBEncode
