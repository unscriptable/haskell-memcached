module Network.Memcache.Helpers (
    cachedOp
) where

import Network.Memcache.Serializable (Serializable)
import Network.Memcache.Key (Key, toKey)
import Network.Memcache.Server (Server, NoConnection, configure, connect, disconnect)
import Network.Memcache.Protocol (Connection, Expiry(Seconds))
import Network.Memcache.Memcache (get, set)
import Network (HostName, PortNumber)
import Control.Exception (finally)

{- |
    Given an IO operation, `a -> IO b`, where `a` is keyable and `b` is
    serializable, and given a connected memcached server, `Server Connection`,
    returns an IO operation, `a -> IO b`, that will attempt to cache the
    key-result pair, `(a, b)`.  Future invocations will attempt to use the
    cached key-result pair, `(a, b)`, rather than perform the original IO
    operation.
-}
cachedOp :: (Key k, Serializable v) =>
  (k -> IO v) -> Server Connection -> k -> IO v
cachedOp op sc k = do
    let key = toKey k
    cached <- get sc key
    case cached of
        Just val -> return val
        Nothing  -> do
            val <- op k
            _ <- set sc key val -- TODO: log if a set is unsuccessful?
            return val
