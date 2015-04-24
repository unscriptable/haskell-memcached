-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.Protocol where

-- TODO:
--  - use exceptions where appropriate for protocol errors
--  - expiration time in store

import qualified Network
import Network.Memcache.Key
import Network.Memcache.Serializable
import System.IO
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word

-- | Gather results from action until condition is true.
ioUntil :: (a -> Bool) -> IO a -> IO [a]
ioUntil stop io = do
  val <- io
  if stop val then return []
              else do more <- ioUntil stop io
                      return (val:more)

-- | Put out a line with \r\n terminator.
hPutNetLn :: Handle -> String -> IO ()
hPutNetLn h str = hPutStr h (str ++ "\r\n")

-- | Put out a line with \r\n terminator.
hBSPutNetLn :: Handle -> ByteString -> IO ()
hBSPutNetLn h str = B.hPutStr h str >> hPutStr h "\r\n"

-- | Get a line, stripping \r\n terminator.
hGetNetLn :: Handle -> IO [Char]
hGetNetLn h = fmap init (hGetLine h) -- init gets rid of \r

-- | Put out a command (words with terminator) and flush.
hPutCommand :: Handle -> [String] -> IO ()
hPutCommand h strs = hPutNetLn h (unwords strs) >> hFlush h

type Flags = Word32

newtype Connection = Connection { sHandle :: Handle }

-- connect :: String -> Network.Socket.PortNumber -> IO Connection
connect :: Network.HostName -> Network.PortNumber -> IO Connection
connect host port = do
  handle <- Network.connectTo host (Network.PortNumber port)
  return (Connection handle)

disconnect :: Connection -> IO ()
disconnect = hClose . sHandle

stats :: Connection -> IO [(String, String)]
stats (Connection handle) = do
  hPutCommand handle ["stats"]
  statistics <- ioUntil (== "END") (hGetNetLn handle)
  return $ map (tupelize . stripSTAT) statistics where
    stripSTAT ('S':'T':'A':'T':' ':x) = x
    stripSTAT x                       = x
    tupelize line = case words line of
                      (key:rest) -> (key, unwords rest)
                      []         -> (line, "")

store :: (Key k, Serializable s) => String -> Connection -> Word32 -> Flags -> k -> s -> IO Bool
store action (Connection handle) exptime flags key val = do
  let valstr = serialize val
  let bytes = B.length valstr
  let cmd = unwords [action, toKey key, show flags, show exptime, show bytes]
  hPutNetLn handle cmd
  hBSPutNetLn handle valstr
  hFlush handle
  response <- hGetNetLn handle
  return (response == "STORED")

getOneValue :: Handle -> IO (Maybe ByteString)
getOneValue handle = do
  s <- hGetNetLn handle
  case words s of
    ["VALUE", _, _, sbytes] -> do
      let count = read sbytes
      val <- B.hGet handle count
      return $ Just val
    _ -> return Nothing

incDec :: (Key k) => String -> Connection -> Word32 -> k -> Word32 -> IO (Maybe Int)
incDec cmd (Connection handle) exptime key delta = do
  hPutCommand handle [cmd, toKey key, show delta]
  response <- hGetNetLn handle
  case response of
    "NOT_FOUND" -> return Nothing
    x           -> return $ Just (read x)

get (Connection handle) key = do
  hPutCommand handle ["get", toKey key]
  val <- getOneValue handle
  case val of
    Nothing -> return Nothing
    Just val -> do
      hGetNetLn handle
      hGetNetLn handle
      return $ deserialize val

delete (Connection handle) key = do
  hPutCommand handle ["delete", toKey key]
  response <- hGetNetLn handle
  return (response == "DELETED")


-- vim: set ts=2 sw=2 et :
