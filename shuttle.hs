{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-imports -fwarn-unused-matches #-}

import System.Linux.Input.Event -- linux-evdev
import Data.Int
import System.IO
import System.Process

data State = State {
  dial :: Int32,
  wheel :: Int32,
  mode :: Int32
} deriving (Eq, Show)

data Button = ButtonLeft
            | ButtonMiddle
            | ButtonRight
            | ButtonWheelUp
            | ButtonWheelDown

handleEvent :: Event -> State -> IO State

handleEvent KeyEvent{ evKeyCode = k, evKeyEventType = Depressed } s
  | k == btn_4 = return s{ mode = 0 }
  | k == btn_5 = return s{ mode = 1 }
  | k == btn_6 = return s{ mode = 2 }
  | k == btn_7 = return s{ mode = 3 }
  | k == btn_8 = return s{ mode = 4 }
  | k == btn_9 = return s{ mode = 5 }

handleEvent RelEvent{ evRelAxis = a, evValue = v } s
  | a == rel_wheel = return s{ wheel = v }
  | a == rel_dial = do
                      case compare v (dial s) of
                        LT -> clickAction ButtonWheelUp
                        GT -> clickAction ButtonWheelDown
                        EQ -> return ()
                      return s{ dial = v }

handleEvent _ s = return s

deviceFile = "/dev/input/by-id/usb-Contour_Design_ShuttleXpress-event-if00"

initialState :: State
initialState = State {
  dial = 0,
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

mainLoop :: Handle -> State -> IO ()
mainLoop h oldState = do
  ee <- hReadEvent h
  case ee of
    Just e -> do
      newState <- handleEvent e oldState
      putStrLn $ show $ e
      putStrLn $ show newState
      putStrLn "----"
      mainLoop h newState
    Nothing -> mainLoop h oldState

main :: IO ()
main = do
  handle <- openBinaryFile deviceFile ReadMode
  mainLoop handle initialState
  return ()
