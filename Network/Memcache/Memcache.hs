module Network.Memcache.Memcache where

import Network.Memcache.Key
import Network.Memcache.Serializable
import Data.Word

class Memcache a where
  set, add, replace :: (Key k, Serializable s) => a -> k -> s -> IO Bool
  get               :: (Key k, Serializable s) => a -> k -> IO (Maybe s)
  delete            :: (Key k) => a -> k -> IO Bool
  incr, decr        :: (Key k) => a -> k -> Word32 -> IO (Maybe Int)
