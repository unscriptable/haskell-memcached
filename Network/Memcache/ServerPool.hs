-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.ServerPool where

import qualified Network.Memcache as M

data Server = Server M.Server Int
data Pool = Pool (String -> Int)

-- vim: set ts=2 sw=2 et :
