module Network.Memcached.Cluster (
    Cluster(..),
    Distribute,
    simple
) where

import Network.Memcached.Server
import Network.Memcached.Key
import Network.Memcached.Memcached

import Data.List (foldl')

data Cluster =
  Cluster {
    servers :: [Server AutoConnection],
    distribute :: Distribute
  }

type Distribute = [Server AutoConnection] -> String -> Server AutoConnection

-- TODO: ketama (requires an MD5 library)
-- Reference: http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients
-- ketama :: String -> [Server NoConnection] -> Server NoConnection
-- ketama k (s : ss) = s -- temporary: get first

-- | A simple distribution function.  Creates a ridiculously simple integer hash
--  from a key and distributes it onto the list of servers.  This algorithm
--  likely creates a "lumpy" distribution and might not be performant on very
--  long keys.
-- TODO: prevent empty list of servers from getting here (how?)
simple :: Distribute
simple [s] _ = s -- single server special case
simple ss k       = head . snd $ splitAt idx ss
  where
    idx = (hash . toKey) k `mod` length ss
    hash = foldl' (\c i -> i + c * 31) 0 . map fromEnum

instance Memcached Cluster where
  get cl k     = get (getServer cl k) k
  delete cl k  = delete (getServer cl k) k
  set cl k     = set (getServer cl k) k
  add cl k     = add (getServer cl k) k
  replace cl k = replace (getServer cl k) k
  incr cl k    = incr (getServer cl k) k
  decr cl k    = decr (getServer cl k) k

getServer :: (Key k) => Cluster -> k -> Server AutoConnection
getServer cl key = distribute cl (servers cl) (toKey key)
