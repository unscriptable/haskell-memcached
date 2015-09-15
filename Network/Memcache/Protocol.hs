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
import Data.Time
import Data.Time.Clock.POSIX
import Control.Applicative ((<$>))

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

data Expiry =
  Never |
  Seconds Word32 |
  Date UTCTime
  deriving (Show)
-- figure out how to limit seconds to the memcached limit of 30 days.

newtype Connection = Connection { sHandle :: Handle }

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

store :: (Key k, Serializable s) => String -> Connection -> Expiry -> Maybe Flags -> k -> s -> IO Bool
store action (Connection handle) expiry flags key val = do
  let valstr = serialize val
  let bytes = B.length valstr
  exptime <- expiryToWord expiry
  let cmd = unwords [action, toKey key, showFlags flags, show exptime, show bytes]
  hPutNetLn handle cmd
  hBSPutNetLn handle valstr
  hFlush handle
  response <- hGetNetLn handle
  -- hPutStr stderr $ "command: " ++ cmd ++ "\n"
  -- hPutStr stderr $ "response: " ++ response ++ "\n"
  return (response == "STORED")

getOneValue :: Handle -> IO (Maybe ByteString)
getOneValue handle = do
  s <- hGetNetLn handle
  -- hPutStr stderr $ "received: " ++ s ++ "\n"
  case words s of
    ["VALUE", _, _, sbytes] -> do
      let count = read sbytes
      val <- B.hGet handle count
    --   hPutStr stderr $ "val: "
    --   hPutStr stderr $ show val
    --   hPutStr stderr $ "\n"
      return $ Just val
    _ -> return Nothing

incDec :: (Key k) => String -> Connection -> k -> Word32 -> IO (Maybe Int)
incDec cmd (Connection handle) key delta = do
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

expiryToWord :: Expiry -> IO Word32
expiryToWord expiry = do
  case expiry of
    Never     -> return 0
    Date d    -> return $ floor $ utcTimeToPOSIXSeconds d
    Seconds s -> safeMemcachedSeconds s

thirtyDays = 30 * 24 * 60 * 60

safeMemcachedSeconds :: Word32 -> IO Word32
safeMemcachedSeconds seconds = do
  if seconds <= thirtyDays
    -- fits within memcached "relative" range
    then return seconds
    -- "absolute" range. convert to a Unix time
    else (+ seconds) . floor <$> getPOSIXTime

showFlags Nothing = "0"
showFlags (Just f) = show f

-- vim: set ts=2 sw=2 et :
