{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Memcache.Server (
  Server(..),
  NoConnection,
  configure,
  connect,
  disconnect
) where

import qualified Network.Memcache.Protocol as P
import qualified Network.Memcache.Memcache as MC

import Control.Exception (finally)
import Network (HostName, PortNumber)

{- TODO:
    - consider some of these updates:
        https://github.com/olegkat/haskell-memcached/compare/master...beketa:5cdadd8941c78137715ac0a340aa69c86ccbdb94
    - or these high-performance networking patterns:
        https://hackage.haskell.org/package/memcache
-}

-- | A memcached server connection.
-- Sample creation:
-- > let proto = configure Never Nothing
-- mc <- connect "my-mc-server"
-- Reference: https://github.com/memcached/memcached/blob/master/doc/protocol.txt
data Server c where
  Disconnected {
    dExpiry :: P.Expiry, -- ^ expiration
    dFlags :: Maybe P.Flags -- ^ extra bits stored with each key-value
  } :: Server NoConnection
  Connected {
    cConn :: P.Connection, -- ^ connection
    cExpiry :: P.Expiry, -- ^ expiration
    cFlags :: Maybe P.Flags -- ^ extra bits stored with each key-value
    -- ^ Note: must be memcached 1.2.1 and higher to support 32 bits
  } :: Server P.Connection
  AutoConnected {
    aConn :: AutoConnection,
    aExpirty :: P.Expiry,
    aFlags :: Maybe P.Flags
  } :: Server AutoConnection

data NoConnection
data AutoConnection = AutoConnection HostName (Maybe PortNumber)

defaultPort = 11211 :: PortNumber

configure :: P.Expiry -> Maybe P.Flags -> Server NoConnection
configure = Disconnected

connect :: Server NoConnection -> HostName -> Maybe PortNumber
    -> IO (Server P.Connection)
connect server host Nothing = connect server host (Just defaultPort)
connect (Disconnected expiry flags) host (Just port) = do
  conn <- P.connect host port
  return $ Connected conn expiry flags

autoConnect :: Server AutoConnection
    -> (Server P.Connection -> IO v) -> IO v
autoConnect (AutoConnected (AutoConnection host port) expiry flags) op = do
    let sn = configure expiry flags
    sc <- connect sn host port
    op sc `finally` disconnect sc

disconnect :: Server P.Connection -> IO (Server NoConnection)
disconnect s@(Connected c e f) = do
  -- sConn implies Connected
  (P.disconnect . cConn) s >> return (Disconnected e f)

instance MC.Memcache (Server P.Connection) where
  get                       = P.get . cConn
  delete                    = P.delete . cConn
  set (Connected c e f)     = P.store "set" c e f
  add (Connected c e f)     = P.store "add" c e f
  replace (Connected c e f) = P.store "replace" c e f
  incr                      = P.incDec "incr" . cConn
  decr                      = P.incDec "decr" . cConn

instance MC.Memcache (Server AutoConnection) where
  get s k        = autoConnect s ((flip MC.get) k)
  delete s k     = autoConnect s ((flip MC.delete) k)

  -- set, add, replace :: a -> k -> s -> IO Bool
  set s k v       = autoConnect s (op k v)
    where op = (\ k v c -> MC.set c k v )
  add s k v       = autoConnect s (op k v)
    where op = (\ k v c -> MC.add c k v )
  replace s k v       = autoConnect s (op k v)
    where op = (\ k v c -> MC.replace c k v )

  incr s k i = autoConnect s (op k i)
    where op = (\ k i c -> MC.incr c k i )
  incr s k i = autoConnect s (op k i)
    where op = (\ k i c -> MC.decr c k i )
