module Network.Memcached.Memcached where

import Network.Memcached.Key
import Network.Memcached.Serializable
import Data.Word

class Memcached a where
  set, add, replace :: (Key k, Serializable v) => a -> k -> v -> IO Bool
  get               :: (Key k, Serializable v) => a -> k -> IO (Maybe v)
  delete            :: (Key k) => a -> k -> IO Bool
  incr, decr        :: (Key k) => a -> k -> Word32 -> IO (Maybe Int)
