{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GADTs, RankNTypes #-}
module PadKontrol (PadKontrol, runPadKontrol, module PadKontrol.Types, module Control.Monad.IO.Class) where

import qualified System.MIDI as MIDI
import PadKontrol.Types
import Control.Applicative
import Control.Concurrent
import Control.Monad.Operational.Mini
import Control.Monad.IO.Class
import Data.Word
import Control.Lens
import qualified Data.Map as M
import Data.Char

type PadKontrol = Program Message

instance MonadIO PadKontrol where
    liftIO = singleton . LiftIO

showHex :: Word8 -> String
showHex n = intToDigit (fromEnum $ n `div` 16) : intToDigit (fromEnum $ n `mod` 16) : ""

convertEvent :: MIDI.MidiEvent -> Maybe (Int, Event)
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x45,s,t]))
    | s <= 0x0F = Just (fromIntegral time, PadUp (toEnum $ fromEnum s))
    | s >= 0x40 = Just (fromIntegral time, PadDown (toEnum $ fromEnum (s - 0x40)) (fromIntegral (t - 0x30)))
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x48,0x20,0x7f])) = Just (fromIntegral time, XYTouch)
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x48,0x20,0x00])) = Just (fromIntegral time, XYRelease)
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x48,s,0x7f])) = Just (fromIntegral time, ButtonDown (toEnum $ fromEnum s))
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x48,s,0x00])) = Just (fromIntegral time, ButtonUp (toEnum $ fromEnum s))
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x49,0x00,v])) = Just (fromIntegral time, Knob1 (fromIntegral v / 127))
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x49,0x01,v])) = Just (fromIntegral time, Knob2 (fromIntegral v / 127))
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x4B,x,y])) = Just (fromIntegral time, XYPad (fromIntegral x / 127) (fromIntegral y / 127))
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x43,0x00,0x01])) = Just (fromIntegral time, JogCW)
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x43,0x00,0x7F])) = Just (fromIntegral time, JogCCW)
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x40,0x00,_])) = Nothing
convertEvent (MIDI.MidiEvent time (MIDI.SysEx [_,_,_,_,0x5f,_,_])) = Nothing
convertEvent (MIDI.MidiEvent _ (MIDI.SysEx xs)) = error $ "unknown event: " ++ unwords (map showHex xs)

runPadKontrol :: (Int -> Event -> Program Message ()) -> ((forall r. Program Message r -> IO r) -> IO a) -> IO a
runPadKontrol handle m = do
    let g getName = fmap M.fromList . mapM (liftA2 (,) <$> getName <*> return)
    srcs <- MIDI.enumerateSources >>= g MIDI.getName
    dests <- MIDI.enumerateDestinations >>= g MIDI.getName
    let devSrc = srcs ^?! ix "padKONTROL PORT A"
        devDest = dests ^?! ix "padKONTROL CTRL"
    dest <- MIDI.openDestination devDest
    src <- MIDI.openSource devSrc $ Just
        $ interpret (eval dest) . maybe (return ()) (uncurry handle) . convertEvent
    MIDI.start dest
    MIDI.start src
    MIDI.sendSysEx dest $ [0x42, 0x49, 0x6E, 0x08, 0x00, 0x00, 0x01]
    MIDI.sendSysEx dest $ [0x42, 0x49, 0x6E, 0x08, 0x3F, 0x0A, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x29, 0x29, 0x29]
    MIDI.sendSysEx dest $
        [ 0x42, 0x49, 0x6E, 0x08, 0x3F, 0x2A, 0x00, 0x00
        , 0x05, 0x05, 0x05, 0x7F, 0x7E, 0x7F, 0x7F, 0x03
        , 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A
        , 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A
        , 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08
        , 0x09, 0x0A, 0x0B, 0x0C, 0x0d, 0x0E, 0x0F, 0x10]
    result <- m $ interpret (eval dest)
    MIDI.sendSysEx dest $ [0x42, 0x40, 0x6E, 0x08, 0x00, 0x00, 0x00]
    MIDI.stop src
    MIDI.stop dest
    MIDI.close src
    MIDI.close dest
    return undefined
    where
        light :: Light -> Word8
        light Off = 0x00
        light On = 0x20
        light Blink = 0x63
        light (Flash f) = 0x41 + floor (f * (0x5f - 0x41))

        eval :: MIDI.Connection -> Message a -> IO a
        eval dest (Display x y z) = send dest [0x22, 0x04, 0x00, x, y, z]
        eval dest (DisplayBlink x y z) = send dest [0x22, 0x04, 0x01, x, y, z]
        eval dest (DisplayLeft seg l) = send dest [0x01, 0xB8 + fromIntegral (fromEnum seg), light l]
        eval dest (DisplayCenter seg l) = send dest [0x01, 0xB0 + fromIntegral (fromEnum seg), light l]
        eval dest (DisplayRight seg l) = send dest [0x01, 0xA8 + fromIntegral (fromEnum seg), light l]
        eval dest (PadLight p l) = send dest [0x01, 0x00 + fromIntegral (fromEnum p), light l]
        eval dest (ButtonLight b l) = send dest [0x01, 0x10 + fromIntegral (fromEnum b), light l]
        eval _ (LiftIO m) = m
        eval dest (AllLight w x y z) = send dest [0x3F, 0x0A, 0x01, g0, g1, g2, g3, g4, 0x00, x, y, z]
        　　where
            g = foldr (flip $ flip (+) . (*2)) 0 . map (toEnum . fromEnum . w) 
            g0 = g $ map Left [Pad01 .. Pad07]
            g1 = g $ map Left [Pad08 .. Pad14]
            g2 = g $ map Left [Pad15 .. Pad16] ++ map Right [ButtonScene ..ButtonMidiCh]
            g3 = g $ map Right [ButtonSWType ..ButtonX]
            g4 = g $ map Right [ButtonY .. ButtonHold]


        send dest = MIDI.sendSysEx dest . ([0x42, 0x40, 0x6E, 0x08]++)
