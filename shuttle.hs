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

scrollDown = createProcess $ proc "xdotool" ["click", "5"]

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
  _ <- scrollDown
  mainLoop h

main :: IO ()
main = do
  handle <- openBinaryFile deviceFile ReadMode
  mainLoop handle
  return ()
