module Network.Memcached.Serializable(Serializable, serialize, deserialize) where

import Data.ByteString (ByteString)
import Codec.Binary.UTF8.Light (encode, decode)

class Serializable a where
  serialize    :: a -> ByteString
  deserialize  :: ByteString -> Maybe a

  serializeL   :: [a] -> ByteString
  deserializeL :: ByteString -> [a]

  serializeL   = error "unimp"
  deserializeL = error "unimp"

instance Serializable Char where
  -- people will rarely want to serialize a single char,
  -- but we define them for completeness.
  serialize   x      = encode [x]
  deserialize s      =
      case decode s of
          (c:[]) -> Just c
          _ -> Nothing

  -- the real use is for serializing strings.
  serializeL   = encode
  deserializeL = decode

instance Serializable ByteString where
    serialize   = id
    deserialize = Just

-- ...do I really need to copy everything instance of Show?
instance Serializable Int where
  serialize   = encode . show
  deserialize = Just . read . decode

instance (Serializable a) => Serializable [a] where
  serialize   = serializeL
  deserialize = Just . deserializeL

-- vim: set ts=2 sw=2 et :
