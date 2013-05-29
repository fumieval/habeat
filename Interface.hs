{-# LANGUAGE LambdaCase #-}
module Interface where
import Types
import Control.Concurrent.MVar
import Control.Concurrent
import PadKontrol

acceptEvent :: MVar World -> Int -> Event -> PadKontrol ()
acceptEvent mw _ event = stateToMVar mw $ do
    mode <- use uiMode
    case (mode, event) of
        (_, ButtonDown ButtonScene) -> uiMode .= SelectPart Nothing
        (SelectPart _, PadDown p _) -> uiMode .= SelectPart (Just (fromEnum p))
        (SelectPart Nothing, ButtonUp ButtonScene) -> uiMode .= Master
        (SelectPart (Just i), ButtonUp ButtonScene) -> uiMode .= EditPart i
        (EditPart i, PadDown p _) -> tracks . ix i . pattern . ix (fromEnum p) %= not
        (Master, PadDown p _) -> trigger (fromEnum p)
        (EditPart i, ButtonDown ButtonMessage) -> trigger i
        (_, ButtonDown ButtonHold) -> playMode %= maybe (Just (Play, 0))
            (Just . over _1 (cataPlayMode Pause Play Pause))
        (_, ButtonDown ButtonFlam) -> playMode .= Nothing
        (_, ButtonDown ButtonRoll) -> playMode %= maybe (Just (Record, 0))
            (Just . over _1 (cataPlayMode Record Record Play))
        (_, ButtonDown ButtonSetting) -> endFlag .= True
        (EditPart i, Knob1 f) -> tracks . ix i . _Volume .= realToFrac f
        (EditPart i, Knob2 f) -> tracks . ix i . _Pan .= realToFrac f
        _ -> return ()
    where
        trigger t = do
            padLight (toEnum t) (Flash 0)
            tracks . ix t . pending %= (True:)
            use playMode >>= \case
                Just (Record, i) -> do
                    c <- use clock
                    c' <- use clockDur
                    if c < div c' 2
                        then tracks . ix t . pattern . ix (succ i `mod` 16) .= True
                        else tracks . ix t . pattern . ix (i `mod` 16) .= True
                    tracks . ix t . tempMute .= True
                _ -> return ()
indicate :: MVar World -> PadKontrol ()
indicate mw = do
    world <- liftIO $ readMVar mw
    case view uiMode world of
        EditPart i -> do
            forM_ [0..15] $ \j -> do
                if world ^?! tracks . ix i . pattern . ix j
                    then padLight (toEnum j) On
                    else padLight (toEnum j) Off
            buttonLight ButtonScene On
        SelectPart (Just i) -> do
            buttonLight ButtonScene Blink
            forM_ [0..15] $ \j -> do
                if i == j
                    then padLight (toEnum j) On
                    else padLight (toEnum j) Off
        SelectPart Nothing -> buttonLight ButtonScene On
        _ -> do
            forM_ [0..15] $ \j -> padLight (toEnum j) Off
            buttonLight ButtonScene Off
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
    
    liftIO $ threadDelay $ 50 * 1000
    unless (view endFlag world) (indicate mw)
