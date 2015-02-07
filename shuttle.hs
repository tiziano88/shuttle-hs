{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-imports -fwarn-unused-matches #-}

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get
import Data.Int
import System.IO
import System.Process

data Event = Event {
  timeSeconds :: Int64,
  timeMicroSeconds :: Int64,
  etype :: Int16,
  code :: Int16,
  value :: Int32
} deriving (Eq, Show)

data State = State {
  jog :: Int32,
  wheel :: Int32,
  mode :: Int32
} deriving (Eq, Show)

data Action = ScrollUp
            | ScrollDown
            | Click

data Button = ButtonLeft
            | ButtonMiddle
            | ButtonRight
            | ButtonWheelUp
            | ButtonWheelDown

handleEvent :: Event -> State -> IO State
handleEvent Event{ etype = 1, code = 260, value = 1 } s = return s{ mode = 0 }
handleEvent Event{ etype = 1, code = 261, value = 1 } s = return s{ mode = 1 }
handleEvent Event{ etype = 1, code = 262, value = 1 } s = return s{ mode = 2 }
handleEvent Event{ etype = 1, code = 263, value = 1 } s = return s{ mode = 3 }
handleEvent Event{ etype = 1, code = 264, value = 1 } s = return s{ mode = 4 }
handleEvent Event{ etype = 2, code = 7, value = v } s = do
  case compare v (wheel s) of
    LT -> clickAction ButtonWheelUp
    GT -> clickAction ButtonWheelDown
    EQ -> return ()
  return s{ wheel = v }
handleEvent Event{ etype = 2, code = 8, value = v } s = return s{ jog = v }
handleEvent _ s = return s

deviceFile = "/dev/input/by-id/usb-Contour_Design_ShuttleXpress-event-if00"

initialState :: State
initialState = State {
  jog = 0,
  wheel = 0,
  mode = 0
}

clickAction :: Button -> IO ()
clickAction b = xdotool ["click", show bb]
  where bb = case b of
             ButtonLeft -> 1
             ButtonMiddle -> 2
             ButtonRight -> 3
             ButtonWheelUp -> 4
             ButtonWheelDown -> 5

xdotool :: [String] -> IO ()
xdotool arg = do
  createProcess $ proc "xdotool" arg
  return ()

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

mainLoop :: Handle -> State -> IO ()
mainLoop h oldState = do
  buf <- B.hGet h 24
  let e = runGet deserialize buf
  newState <- handleEvent e oldState
  putStrLn $ show $ e
  putStrLn $ show newState
  putStrLn "----"
  mainLoop h newState

main :: IO ()
main = do
  handle <- openBinaryFile deviceFile ReadMode
  mainLoop handle initialState
  return ()
