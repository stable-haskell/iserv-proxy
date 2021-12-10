{-# LANGUAGE GADTs, StandaloneDeriving, ExistentialQuantification #-}

module IServ.Remote.Message
  ( ProxyMessage(..)
  , SomeProxyMessage(..)
  , putProxyMessage
  , getProxyMessage )
where

import GHC.Fingerprint (Fingerprint)
import Data.Binary
import Data.ByteString (ByteString)

-- | A @ProxyMessage a@ is message from the iserv process on the
-- target, requesting something from the Proxy of with result type @a@.
data ProxyMessage a where
  -- sends either a new file, or nothing if the file is acceptable.
  Have     :: FilePath -> Fingerprint -> ProxyMessage (Maybe ByteString)
  Missing  :: FilePath -> ProxyMessage ByteString
  Done     :: ProxyMessage ()

deriving instance Show (ProxyMessage a)

putProxyMessage :: ProxyMessage a -> Put
putProxyMessage m = case m of
  Have path sha  -> putWord8 0 >> put path >> put sha
  Missing path   -> putWord8 1 >> put path
  Done           -> putWord8 2

data SomeProxyMessage where
    SomeProxyMessage :: forall a. (Binary a, Show a) => ProxyMessage a -> SomeProxyMessage

getProxyMessage :: Get SomeProxyMessage
getProxyMessage = do
  b <- getWord8
  case b of
    0 -> SomeProxyMessage <$> (Have   <$> get <*> get)
    1 -> SomeProxyMessage <$> Missing <$> get
    2 -> return (SomeProxyMessage Done)
