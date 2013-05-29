{-# LANGUAGE Rank2Types, OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -O2 -fexcess-precision #-}
import Control.Concurrent
import Linear
import qualified Data.IntMap as IM
import Sound.PortAudio
import Sound.PortAudio.Base
import Foreign.Storable
import System.Environment
import Control.Lens.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Types
import PadKontrol
import Process
import Interface
import Data.Text.Lens
import qualified Data.Aeson as JSON

runBeat :: Int -> StateT World PadKontrol ()
runBeat n = do
    mode <- use uiMode
    case mode of
        EditPart _ -> padLight (toEnum n) (Flash 0)
        _ -> return ()
    
    ts <- use tracks
    (>>=assign tracks) $ iforM ts $ \i s -> flip execStateT s $ do
        Just b <- preuse $ pattern . ix n
        when b $ do
            case mode of
                Master -> padLight (toEnum i) (Flash 0)
                _ -> return ()
            use tempMute >>= \case
                True -> tempMute .= False
                False -> pending %= (True:)
    return ()

play :: (forall a. PadKontrol a -> IO a) -> MVar World -> Int -> IO ()
play runner mw b = do
    world <- takeMVar mw
    if view clock world == 0
        then do
            world' <- runner $ execStateT (runBeat b) world
            putMVar mw $ world' & clock .~ view clockDur world & beat %~ (`mod`16) . (+1)
    else putMVar mw $ world & clock -~ 1

audioCallback :: (forall a. PadKontrol a -> IO a) -> MVar World -> StreamCallback CFloat CFloat
audioCallback runner mw _ _ frames _ out = forM [0..fromIntegral frames-1] write >> return Continue where
    write bi = do
        V2 l r <- fmap (foldrOf folded (+) zero) $ do
            ts <- stateToMVar mw $ use tracks
            iforM ts $ \i s -> do
                (a, s') <- flip runStateT s runTrack
                stateToMVar mw $ tracks . ix i .= s'
                return (a * view trackGain s)

        pokeElemOff out (2 * bi) l
        pokeElemOff out (2 * bi + 1) r

        world <- readMVar mw
        case world ^. playMode of
            Just (Play, b) -> play runner mw b
            Just (Record, b) -> play runner mw b
            _ -> return ()

appMain :: MVar World -> IO (Either Error ())
appMain mw = do
    -- let devi = StreamParameters (PaDeviceIndex 5) 2 (PaTime 0.12)
    i <- fromIntegral <$> deviceId <$> readMVar mw
    let devo = StreamParameters (PaDeviceIndex i) 2 (PaTime 0.0)

    withPortAudio $ runPadKontrol (acceptEvent mw)
        $ \runner -> withStream
            Nothing
            (Just devo)
            44100
            (Just 256)
            []
            (Just $ audioCallback runner mw)
            (Just $ return ())
            $ \stream -> do
                _ <- startStream stream
                runner $ indicate mw
                _ <- stopStream stream
                return $ Right ()

loadProject :: FilePath -> IO World
loadProject path = do
    Just v <- preview _JSON <$> BS.readFile path :: IO (Maybe JSON.Value)
    ts <- forM (v ^?! ix "tracks" . _Object . to itoList) $ \(name, t) -> do
        putStrLn $ "loading track: " ++ review packed name 
        s <- sampler (t ^?! ix "path" . _String . from packed)
        return (t ^?! ix "index" . _Integer . from enum, Track
            { _trackName = name ^. from packed
            , _trackVein = s
            , _pending = []
            , _pattern = V.fromList $ t ^.. ix "pattern" . _Array . traversed . _Integer . to (/=0)
            , _trackGain = V2 (t ^?! ix "gainL" . _Double . to realToFrac) (t ^?! ix "gainR" . _Double . to realToFrac)
            , _tempMute = False
            })
    return $ World {
            _endFlag = False
            , _uiMode = Master
            , _playMode = Nothing
            , _clockDur = floor $ 44100 * 60 / (v ^?! key "bpm" . _Double) / 4
            , _clock = 0
            , _tracks = IM.fromList ts
            , deviceId = v ^?! ix "device" . _Integer . from enum
        }

main :: IO ()
main = getArgs >>= \case
    ("open" : path : _) -> loadProject path >>= newMVar >>= appMain >>= print
    ("devices" : _) -> void $ withPortAudio $ do
        n <- getNumDevices
        forM_ [0..n-1] $ \i -> do
            putStrLn $ "Device: " ++ show i
            info <- getDeviceInfo (PaDeviceIndex $ fromIntegral i) >>= either (fail.show) return
            putStrLn (name_PaDeviceInfo info)
            print (defaultSampleRate info)
            putStrLn ""

        return (Right ())
    _ -> putStrLn "Usage: habeat open [path] | habeat devices"