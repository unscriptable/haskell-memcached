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

data NoConnection

defaultPort = 11211 :: PortNumber

configure :: P.Expiry -> Maybe P.Flags -> Server NoConnection
configure = Disconnected

connect :: Server NoConnection -> HostName -> Maybe PortNumber
    -> IO (Server P.Connection)
connect server host Nothing = connect server host (Just defaultPort)
connect (Disconnected expiry flags) host (Just port) = do
  conn <- P.connect host port
  return $ Connected conn expiry flags

disconnect :: Server P.Connection -> IO (Server NoConnection)
disconnect s@(Connected c e f) = do
  -- sConn implies Connected
  (P.disconnect . cConn) s >> return (Disconnected e f)

instance MC.Memcache (Server P.Connection) where
  get                       = P.get . cConn -- implies Connected
  delete                    = P.delete . cConn -- implies Connected
  set (Connected s e f)     = P.store "set" s e f
  add (Connected s e f)     = P.store "add" s e f
  replace (Connected s e f) = P.store "replace" s e f
  incr (Connected s _ _)    = P.incDec "incr" s
  decr (Connected s _ _)    = P.incDec "decr" s
