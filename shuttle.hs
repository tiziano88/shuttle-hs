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

data Action = ScrollUp
            | ScrollDown
            | Click

data Button = ButtonLeft | ButtonMiddle | ButtonRight | ButtonWheelUp | ButtonWheelDown

deviceFile = "/dev/input/by-id/usb-Contour_Design_ShuttleXpress-event-if00"

clickAction :: Button -> [String]
clickAction b = ["click", show bb]
  where bb = case b of
              ButtonLeft -> 1
              ButtonMiddle -> 2
              ButtonRight -> 3
              ButtonWheelUp -> 4
              ButtonWheelDown -> 5

convertAction :: Action -> [String]
convertAction ScrollUp = clickAction ButtonWheelUp
convertAction ScrollDown = clickAction ButtonWheelDown
convertAction Click = clickAction ButtonLeft

performAction :: Action -> IO ()
performAction a = let arg = convertAction a in
  xdotool arg

noAction :: IO ()
noAction = return ()

xdotool :: [String] -> IO ()
xdotool arg = do
  createProcess $ proc "xdotool" arg
  return ()

fromEvent :: Event -> Maybe (IO ())
fromEvent e = case etype e of
                1 -> if value e == 1
                     then case code e of
                            261 -> Just $ performAction ScrollUp
                            262 -> Just $ performAction ScrollDown
                            _ -> Nothing
                     else Nothing
                _ -> Nothing

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
    Just a -> a
    Nothing -> noAction
  mainLoop h

main :: IO ()
main = do
  handle <- openBinaryFile deviceFile ReadMode
  mainLoop handle
  return ()
