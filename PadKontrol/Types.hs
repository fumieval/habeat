{-# LANGUAGE GADTs, FlexibleContexts #-}
module PadKontrol.Types where
import Data.Word
import Control.Monad.Operational.Mini

data Pad = Pad01 | Pad02 | Pad03 | Pad04
         | Pad05 | Pad06 | Pad07 | Pad08
         | Pad09 | Pad10 | Pad11 | Pad12
         | Pad13 | Pad14 | Pad15 | Pad16
          deriving (Show, Read, Eq, Ord, Enum)

data Button = ButtonScene | ButtonMessage | ButtonSetting
    | ButtonNoteCC | ButtonMidiCh | ButtonSWType | ButtonRelVal | ButtonVelocity | ButtonPort
    | ButtonFixedVelocity | ButtonProgChange
    | ButtonX | ButtonY | ButtonKnob1Assign | ButtonKnob2Assign | ButtonPedal
    | ButtonRoll | ButtonFlam | ButtonHold deriving (Show, Read, Eq, Ord, Enum)

data SevenSegment = SegTop
    | SegUpperRight
    | SegLowerRight
    | SegBottom
    | SegLowerLeft
    | SegUpperLeft
    | SegCenter
    | SegPoint
    deriving (Show, Read, Eq, Ord, Enum)

data Light = Off | On | Blink | Flash Float

data Message x where
    Display :: Word8 -> Word8 -> Word8 -> Message ()
    DisplayBlink :: Word8 -> Word8 -> Word8 -> Message ()
    DisplayLeft :: SevenSegment -> Light -> Message ()
    DisplayCenter :: SevenSegment -> Light -> Message ()
    DisplayRight :: SevenSegment -> Light -> Message ()
    PadLight :: Pad -> Light -> Message ()
    ButtonLight :: Button -> Light -> Message ()
    AllLight :: (Either Pad Button -> Bool) -> Word8 -> Word8 -> Word8 -> Message ()
    LiftIO :: IO a -> Message a

display :: Operational Message m => Word8 -> Word8 -> Word8 -> m ()
display x y z = singleton $ Display x y z

displayBlink :: Operational Message m => Word8 -> Word8 -> Word8 -> m ()
displayBlink x y z = singleton $ DisplayBlink x y z

displayLeft :: Operational Message m => SevenSegment -> Light -> m ()
displayLeft s l = singleton $ DisplayLeft s l

displayCenter :: Operational Message m => SevenSegment -> Light -> m ()
displayCenter s l = singleton $ DisplayCenter s l

displayRight :: Operational Message m => SevenSegment -> Light -> m ()
displayRight s l = singleton $ DisplayRight s l

padLight :: Operational Message m => Pad -> Light -> m ()
padLight p l = singleton $ PadLight p l

buttonLight :: Operational Message m => Button -> Light -> m ()
buttonLight b l = singleton $ ButtonLight b l

allLight :: Operational Message m => (Either Pad Button -> Bool) -> Word8 -> Word8 -> Word8 -> m ()
allLight w x y z = singleton $ AllLight w x y z

data Event = PadDown Pad Int
    | PadUp Pad
    | ButtonDown Button
    | ButtonUp Button
    | Knob1 Float
    | Knob2 Float
    | XYPad Float Float
    | XYTouch
    | XYRelease
    | JogCW
    | JogCCW
    deriving Show

