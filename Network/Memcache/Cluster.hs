module Network.Memcache.Cluster where

import Network.Memcache.Server
import Network.Memcache.Key
import Network.Memcache.Memcache
import qualified Network.Memcache.Protocol as P

import Data.List (foldl')

data Cluster =
  Cluster {
    servers :: [Server AutoConnection],
    distribute :: Distribute
  }

type Distribute = [Server NoConnection] -> String -> Server NoConnection

choose :: Cluster -> String -> Server NoConnection
choose (Cluster ss dist) keyable = dist ss (toKey keyable)

-- TODO: ketama (requires an MD5 library)
-- Reference: http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients
-- ketama :: String -> [Server NoConnection] -> Server NoConnection
-- ketama k (s : ss) = s -- temporary: get first

-- this seems ok for short keys
-- TODO: prevent empty list of servers from getting here
simple :: String -> [Server NoConnection] -> Server NoConnection
simple k (s : []) = s
simple k ss       = head . snd $ splitAt idx ss
  where
    hash = foldl' (\c i -> i + c * 31) 0 . map fromEnum
    idx = (hash . toKey) k `mod` length ss

instance Memcache Cluster where
  get cl k                  = P.get (getConnection cl k)
  delete cl k               = P.delete (getConnection cl k)
  set cl e f k              = P.store "set" (getConnection cl k) e f
  -- add (Connected s e f)     = P.store "add" s e f
  -- replace (Connected s e f) = P.store "replace" s e f
  -- incr (Connected s _ _)    = P.incDec "incr" s
  -- decr (Connected s _ _)    = P.incDec "decr" s

-- TODO: figure out how to connect the server.  They're all Server NoConnection!
getConnection cl key = cConn (distribute cl (servers cl) key)
