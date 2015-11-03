module Network.Memcached.ServerPool where

import qualified Network.Memcached.Server as S

-- TODO: make sense of this or refactor it

data Pool = Pool [S.Server]
