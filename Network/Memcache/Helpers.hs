module Network.Memcache.Helpers (
    cachedOp,
    autoConnect
) where

import Network.Memcache.Serializable (Serializable)
import Network.Memcache.Key (Key, toKey)
import Network.Memcache.Server (Server, NoConnection, configure, connect, disconnect)
import Network.Memcache.Protocol (Connection, Expiry(Seconds))
import Network.Memcache.Memcache (get, set)
import Network (HostName, PortNumber)
import Control.Exception

{- |
    Given an IO operation, `a -> IO b`, where `a` is keyable and `b` is
    serializable, and given a connected memcached server, `Server Connection`,
    returns an IO operation, `a -> IO b`, that will attempt to cache the
    key-result pair, `(a, b)`.  Future invocations will attempt to use the
    cached key-result pair, `(a, b)`, rather than perform the original IO
    operation.
-}
cachedOp :: (Key a, Serializable b) => (a -> IO b) -> Server Connection -> a -> IO b
cachedOp op sc a = do
    let key = toKey a
    cached <- get sc key
    case cached of
        Just val -> return val
        Nothing  -> do
            val <- op a
            _ <- set sc key val -- TODO: log if a set is unsuccessful?
            return val

{- |
    Given an unconnected memcached server, a host name, a port number,
    transform a function that expects a connected memcached server and an
    IO operation, `a -> IO b`, into an IO operation that automatically
    converts the unconnected server into a connected server and performs
    the original IO operation.
-}
autoConnect :: Server NoConnection -> HostName -> Maybe PortNumber
    -> (Server Connection -> a -> IO b) -> a -> IO b
autoConnect sn host mport op a = do
    sc <- connect sn host mport
    op sc a `finally` disconnect sc

sn = configure (Seconds 1800) Nothing
myCachedOp = autoConnect sn "foo.com" Nothing (cachedOp someOp)

someOp :: String -> IO String
someOp a = do
    print a
    return a
