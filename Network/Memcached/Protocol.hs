module Network.Memcached.Protocol where

import qualified Network
import Network.Memcached.Key
import Network.Memcached.Serializable
import System.IO
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX

type Flags = Word32

data Expiry =
    Never |
    Seconds Word32 | -- ^ Limited at run-time to 1..2592000 seconds (30 days).
    Date UTCTime
    deriving (Show)

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
    return $ map (tupelize . stripSTAT) statistics
    where
        stripSTAT ('S':'T':'A':'T':' ':x) = x
        stripSTAT x                       = x
        tupelize line = case words line of
            (key:rest) -> (key, unwords rest)
            []         -> (line, "")

store :: (Key k, Serializable s) => String -> Connection -> Expiry -> Maybe Flags -> k -> s -> IO Bool
store action (Connection handle) expiry flags key val = do
    let valstr = serialize val
    let bytes = B.length valstr
    let exptime = expiryToWord expiry
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

get :: (Key k, Serializable s) => Connection -> k -> IO (Maybe s)
get (Connection handle) key = do
    hPutCommand handle ["get", toKey key]
    mval <- getOneValue handle
    case mval of
        Nothing -> return Nothing
        Just val -> do
            _ <- hGetNetLn handle
            _ <- hGetNetLn handle
            return $ deserialize val

-- TODO: consider throwing instead of returning bool
delete :: (Key k) => Connection -> k -> IO Bool
delete (Connection handle) key = do
    hPutCommand handle ["delete", toKey key]
    response <- hGetNetLn handle
    return (response == "DELETED")

expiryToWord :: Expiry -> Word32
expiryToWord expiry =
    case expiry of
        Never     -> 0
        Date d    -> floor (utcTimeToPOSIXSeconds d)
        Seconds s -> safeMemcachedSeconds s

thirtyDays :: Word32
thirtyDays = 30 * 24 * 60 * 60

-- | Prevents accidental conversion to other meanings for the `expiry` param.
-- Memcached interprets anything over 30 days to be a unix timestamp and
-- interprets 0 as "never expire".
safeMemcachedSeconds :: Word32 -> Word32
safeMemcachedSeconds seconds = max 1 $ min seconds thirtyDays

showFlags :: Maybe Flags -> String
showFlags Nothing = "0"
showFlags (Just f) = show f

-- | Gather results from action until condition is true.
ioUntil :: (a -> Bool) -> IO a -> IO [a]
ioUntil stop io = do
    val <- io
    if stop val
        then return []
        else do
            more <- ioUntil stop io
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
