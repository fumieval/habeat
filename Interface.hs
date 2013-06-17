{-# LANGUAGE LambdaCase #-}
module Interface where
import Types
import Control.Concurrent.MVar
import Control.Concurrent
import UI.PadKontrol
import System.Mem

acceptEvent :: MVar World -> Int -> Event -> PadKontrol ()
acceptEvent mw _ event = stateToMVar mw $ do
    mode <- use uiMode
    let bar = 0
    case (mode, event) of
        (_, ButtonDown ButtonSetting) -> uiMode .= SelectPart Nothing
        (SelectPart Nothing, ButtonUp ButtonSetting) -> uiMode .= Master
        (SelectPart (Just i), ButtonUp ButtonSetting) -> uiMode .= EditPart i
        (Master, PadDown p _) -> trigger (fromEnum p)
        (SelectPart _, PadDown p _) -> uiMode .= SelectPart (Just (fromEnum p))
        (EditPart i, PadDown p _) -> tracks . ix i . pattern . ix bar . ix (fromEnum p) %= not
        (EditPart i, ButtonDown ButtonPedal) -> trigger i
        (_, ButtonDown ButtonHold) -> playMode %= maybe (Just (Play, 0))
            (Just . over _1 (cataPlayMode Pause Play Pause))
        (_, ButtonDown ButtonFlam) -> playMode .= Nothing
        (_, ButtonDown ButtonRoll) -> playMode %= maybe (Just (Record, 0))
            (Just . over _1 (cataPlayMode Record Record Play))
        (_, ButtonDown ButtonRelVal) -> endFlag .= True
        (EditPart i, Knob1 f) -> tracks . ix i . _Volume .= realToFrac f
        (EditPart i, Knob2 f) -> tracks . ix i . _Pan .= realToFrac f
        (_, JogCW) -> _BPM += 1
        (_, JogCCW) -> _BPM -= 1
        _ -> return ()
    where
        trigger t = do
            padLight (toEnum t) (Flash 0)
            tracks . ix t . pending %= (True:)
            use playMode >>= \case
                Just (Record, i) -> do
                    let b = 0
                    c <- use clock
                    c' <- use clockDur
                    if c < div c' 2
                        then tracks . ix t . pattern . ix b . ix (succ i `mod` 16) .= True
                        else tracks . ix t . pattern . ix b . ix (i `mod` 16) .= True
                    tracks . ix t . tempMute .= True
                _ -> return ()

indicate :: MVar World -> PadKontrol ()
indicate mw = do
    world <- liftIO $ readMVar mw
    case view uiMode world of
        EditPart i -> do
            forM_ [0..15] $ \j -> do
                let b = 0
                if world ^?! tracks . ix i . pattern . ix b . ix j
                    then padLight (toEnum j) On
                    else padLight (toEnum j) Off
            buttonLight ButtonSetting On
        SelectPart (Just i) -> do
            buttonLight ButtonSetting Blink
            forM_ [0..15] $ \j -> do
                if i == j
                    then padLight (toEnum j) On
                    else padLight (toEnum j) Off
        SelectPart Nothing -> buttonLight ButtonSetting On
        _ -> do
            forM_ [0..15] $ \j -> padLight (toEnum j) Off
            buttonLight ButtonSetting Off
    case view playMode world of
        Nothing -> do
            buttonLight ButtonHold Off
            buttonLight ButtonFlam On
            buttonLight ButtonRoll Off
        Just (Play, _) -> do
            buttonLight ButtonHold On
            buttonLight ButtonFlam Off
            buttonLight ButtonRoll Off
        Just (Pause, _) -> do
            buttonLight ButtonHold Blink
            buttonLight ButtonFlam Off
            buttonLight ButtonRoll Off
        Just (Record, _) -> do
            buttonLight ButtonHold On
            buttonLight ButtonFlam Off
            buttonLight ButtonRoll On

    let bpm = floor $ view _BPM world
    display (0x30 + fromIntegral bpm `div` 100) (0x30 + fromIntegral bpm `div` 10 `mod` 10) (0x30 + bpm `mod` 10)
    liftIO $ threadDelay $ 30 * 1000
    unless (view endFlag world) (indicate mw)
