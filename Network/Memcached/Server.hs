{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Memcached.Server (
  Server(..),
  NoConnection,
  AutoConnection,
  configure,
  configureA,
  connect,
  disconnect,
  autoConnect
) where

import qualified Network.Memcached.Protocol as P
import qualified Network.Memcached.Memcached as MC

import Control.Exception (bracket)
import Network (HostName, PortNumber)

{- TODO:
    - consider some of these updates:
        https://github.com/olegkat/haskell-memcached/compare/master...beketa:5cdadd8941c78137715ac0a340aa69c86ccbdb94
    - or these high-performance networking patterns:
        https://hackage.haskell.org/package/memcache
-}

{- TODO: create some examples for this code. -}

-- | A memcached server connection.
-- Sample creation:
-- > let proto = configure Never Nothing
-- mc <- connect "my-mc-server"
-- Reference: https://github.com/memcached/memcached/blob/master/doc/protocol.txt
data Server c where
  Disconnected :: {
    dExpiry :: P.Expiry, -- ^ expiration
    dFlags :: Maybe P.Flags -- ^ extra bits stored with each key-value
  } -> Server NoConnection
  Connected :: {
    cConn :: P.Connection, -- ^ connection
    cExpiry :: P.Expiry, -- ^ expiration
    cFlags :: Maybe P.Flags -- ^ extra bits stored with each key-value
    -- ^ Note: must be memcached 1.2.1 and higher to support 32 bits
  } -> Server P.Connection
  AutoConnected :: {
    aConn :: AutoConnection,
    aExpiry :: P.Expiry,
    aFlags :: Maybe P.Flags
  } -> Server AutoConnection

data NoConnection
data AutoConnection = AutoConnection HostName (Maybe PortNumber)

defaultPort :: PortNumber
defaultPort = 11211

configure :: P.Expiry -> Maybe P.Flags -> Server NoConnection
configure = Disconnected

configureA :: HostName -> Maybe PortNumber -> P.Expiry -> Maybe P.Flags -> Server AutoConnection
configureA h p e f = AutoConnected (AutoConnection h p) e f

connect :: Server NoConnection -> HostName -> Maybe PortNumber
    -> IO (Server P.Connection)
connect server host Nothing = connect server host (Just defaultPort)
connect (Disconnected expiry flags) host (Just port) = do
  conn <- P.connect host port
  return $ Connected conn expiry flags

autoConnect :: Server AutoConnection -> (Server P.Connection -> IO r) -> IO r
autoConnect sa = bracket (connectA sa) disconnect

connectA :: Server AutoConnection -> IO (Server P.Connection)
connectA (AutoConnected (AutoConnection host port) expiry flags) =
  connect (configure expiry flags) host port

disconnect :: Server P.Connection -> IO (Server NoConnection)
disconnect (Connected c e f) = P.disconnect c >> return (Disconnected e f)

instance MC.Memcached (Server P.Connection) where
  get                       = P.get . cConn
  delete                    = P.delete . cConn
  set (Connected c e f)     = P.store "set" c e f
  add (Connected c e f)     = P.store "add" c e f
  replace (Connected c e f) = P.store "replace" c e f
  incr                      = P.incDec "incr" . cConn
  decr                      = P.incDec "decr" . cConn

instance MC.Memcached (Server AutoConnection) where
  get s k       = autoConnect s (\ sc -> MC.get sc k)
  delete s k    = autoConnect s (\ sc -> MC.delete sc k)
  set s k v     = autoConnect s (\ sc -> MC.set sc k v)
  add s k v     = autoConnect s (\ sc -> MC.add sc k v)
  replace s k v = autoConnect s (\ sc -> MC.replace sc k v)
  incr s k i    = autoConnect s (\ sc -> MC.incr sc k i)
  decr s k i    = autoConnect s (\ sc -> MC.decr sc k i)
