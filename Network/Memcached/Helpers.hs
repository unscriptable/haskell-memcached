module Network.Memcached.Helpers (
    cachedOp
) where

import Network.Memcached.Serializable (Serializable)
import Network.Memcached.Key (Key, toKey)
import Network.Memcached.Server (Server, NoConnection, configure, connect, disconnect)
import Network.Memcached.Protocol (Connection, Expiry(Seconds))
import Network.Memcached.Memcached (get, set)
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
