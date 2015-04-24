-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache (
    Server(..),
    Memcache
) where

import qualified Network.Memcache.Protocol as P
import Network.Memcache.Serializable
import Network.Memcache.Key

import Data.Word

-- | A memcached server connection.
data Server = Server {
        conn :: P.Connection, -- ^ connection
        expiry :: P.Expiry, -- ^ expiration
        flags :: P.Flags -- ^ extra bits stored with each key-value
        -- ^ Note: must be memcached 1.2.1 and higher to support 32 bits
    } |
    Simple {
        conn :: P.Connection -- ^ connection
    }

class Memcache a where
  set, add, replace :: (Key k, Serializable s) => a -> k -> s -> IO Bool
  get               :: (Key k, Serializable s) => a -> k -> IO (Maybe s)
  delete            :: (Key k) => a -> k -> IO Bool
  incr, decr        :: (Key k) => a -> k -> Word32 -> IO (Maybe Int)

instance Memcache Server where
  get s                  = P.get (conn s)
  delete s               = P.delete (conn s)
  set (Simple s)         = P.store "set" s P.never 0
  set (Server s e f)     = P.store "set" s e f
  add (Simple s)         = P.store "add" s P.never 0
  add (Server s e f)     = P.store "add" s e f
  replace (Simple s)     = P.store "replace" s P.never 0
  replace (Server s e f) = P.store "replace" s e f
  incr (Simple s)        = P.incDec "incr" s
  incr (Server s _ _)    = P.incDec "incr" s
  decr (Simple s)        = P.incDec "decr" s
  decr (Server s _ _)    = P.incDec "decr" s

-- vim: set ts=2 sw=2 et :
