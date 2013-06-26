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
import UI.PadKontrol
import Process
import Interface
import Data.Text.Lens
import qualified Data.Aeson as JSON
import System.IO.Unsafe
import Data.Function
import Control.Vein
import Vein.Filter
import Control.Bool

theLPF :: MVar (Vein m (Wave, BiquadParam) Wave)
theLPF = unsafePerformIO $ newMVar biQuadFilter

runBeat :: Int -> Int -> StateT World PadKontrol ()
runBeat bar n = do
    mode <- use uiMode
    case mode of
        EditPart _ -> padLight (toEnum n) (Flash 0)
        _ -> return ()
    
    ts <- use tracks
    (>>=assign tracks) $ iforM ts $ \i s -> flip execStateT s $ do
        Just b <- preuse $ pattern . ix bar . ix n
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
            world' <- runner $ execStateT (runBeat 0 b) world
            putMVar mw $ world' & clock .~ view clockDur world & beat %~ (`mod`16) . succ
    else putMVar mw $ world & clock -~ 1

audioCallback :: (forall a. PadKontrol a -> IO a) -> MVar World -> StreamCallback CFloat CFloat
audioCallback runner mw _ _ frames _ out = forM_ [0..fromIntegral frames-1] write >> return Continue where
    write bi = do
        pre <- fmap (IM.foldr (+) zero) $ do
            ts <- view tracks <$> readMVar mw
            iforM ts $ \i s -> do
                (a, s') <- flip runStateT s runTrack
                stateToMVar mw $ tracks . ix i .= s'
                return (a * view trackGain s)

        fx <- view masterFX <$> readMVar mw
        stateToMVar mw $ runVein fx pre $ \(V2 postl postr) cont -> do
            liftIO $ pokeElemOff out (2 * bi) postl
            liftIO $ pokeElemOff out (2 * bi + 1) postr
            masterFX .= cont

        world <- readMVar mw
        case world ^. playMode of
            Just (Play, b) -> play runner mw b
            Just (Record, b) -> play runner mw b
            _ -> return ()

theMaster :: Vein (StateT World IO) Wave Wave
theMaster = Vein $ \i cont -> do
    v <- liftIO (takeMVar theLPF)
    p <- use currentBiquadParam
    runVein v (i, p) $ \o v' -> liftIO (putMVar theLPF v') >> cont o theMaster

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
            , _pattern = [V.fromList $ t ^.. ix "pattern" . _Array . traversed . _Integer . to (/=0)]
            , _trackGain = V2 (t ^?! ix "gainL" . _Double . to realToFrac) (t ^?! ix "gainR" . _Double . to realToFrac)
            , _tempMute = False
            })
    return $ World {
            _endFlag = False
            , _uiMode = Master
            , _playMode = Nothing
            , _clockDur = undefined
            , _clock = 0
            , _tracks = IM.fromList ts
            , _masterFX = theMaster
            , _currentBiquadParam = BiquadParam 1 0 0 1 0 0
            , deviceId = v ^?! ix "device" . _Integer . from enum
        } & _BPM .~ v ^?! ix "bpm" . _Double

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