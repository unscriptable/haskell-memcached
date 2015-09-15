-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.ServerPool where

import qualified Network.Memcache.Server as S

-- TODO: make sense of this or refactor it
-- TODO: consider implementing ketama:
--   http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients
-- TODO: a simple algorithm for now:
--   let server = hash key `mod` length serverlist
-- TODO: inject hash algorithm here instead of as a Key class method:
--    1. create a Distribution class
--    2. implement (instance) a simple algorithm or a ketama algorithm

data Server = Server (S.Server S.NoConnection) Int
data Pool = Pool (String -> Int)

-- vim: set ts=2 sw=2 et :
