module Network.Memcache.ServerPool where

import qualified Network.Memcache.Server as S

-- TODO: make sense of this or refactor it

data Pool = Pool [S.Server]
