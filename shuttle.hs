import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Int
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO
import System.Process

data Event = Event {
  timeSeconds :: Int64,
  timeMicroSeconds :: Int64,
  etype :: Int16,
  code :: Int16,
  value :: Int32
} deriving (Eq, Show)

data Action
  = ScrollUp
  | ScrollDown

convertAction :: Action -> [String]
convertAction ScrollUp = ["click", "5"]
convertAction ScrollDown = ["click", "4"]

performAction :: Action -> IO ()
performAction a = let arg = convertAction a in
  xdotool arg

xdotool :: [String] -> IO ()
xdotool arg = do
  createProcess $ proc "xdotool" arg
  return ()

fromEvent :: Event -> Maybe Action
fromEvent e = case code e of
  261 -> Just ScrollUp
  262 -> Just ScrollDown
  _ -> Nothing

deviceFile = "/dev/input/by-id/usb-Contour_Design_ShuttleXpress-event-if00"

deserialize :: Get Event
deserialize = do
  timeSeconds <- getWord64le
  timeMicroSeconds <- getWord64le
  etype <- getWord16le
  code <- getWord16le
  value <- getWord32le
  return Event {
    timeSeconds = fromIntegral timeSeconds,
    timeMicroSeconds = fromIntegral timeMicroSeconds,
    etype = fromIntegral etype,
    code = fromIntegral code,
    value = fromIntegral value
  }

mainLoop h = do
  buf <- B.hGet h 24
  let e = runGet deserialize buf
  putStrLn $ show $ e
  case fromEvent e of
    Just a -> performAction a
    Nothing -> return ()
  mainLoop h

main :: IO ()
main = do
  handle <- openBinaryFile deviceFile ReadMode
  mainLoop handle
  return ()
