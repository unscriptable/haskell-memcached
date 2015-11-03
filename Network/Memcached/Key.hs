{-# LANGUAGE FlexibleInstances #-}
module Network.Memcached.Key(Key, toKey) where

-- TODO: get rid of this class. it's not actually helpful.

class Key a where
  toKey :: a -> String

-- omit quotes on strings
instance Key [Char] where
  toKey = id
