{-# LANGUAGE TemplateHaskell #-}
module Types (
    Wave
    , Track(..)
    , trackName
    , pattern
    , trackVein
    , pending
    , trackGain
    , tempMute
    , _Volume
    , _Pan
    , UIMode(..)
    , PlayMode(..)
    , cataPlayMode
    , World(..)
    , tracks
    , endFlag
    , uiMode
    , playMode
    , clockDur
    , clock
    , beat
    , stateToMVar
    , currentBiquadParam
    , masterFX
    , _BPM
    , module Vein.Filter
    , module Control.Lens
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.State.Strict
    , module Foreign.C.Types
    )
    where
import Foreign.C.Types
import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Vein
import qualified Data.Vector as V
import Linear
import qualified Data.IntMap as IM
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Vein.Filter

data Track = Track
    { _trackName :: String
    , _pattern :: [V.Vector Bool]
    , _trackVein :: Vein IO Bool Wave
    , _pending :: [Bool]
    , _trackGain :: V2 CFloat
    , _tempMute :: Bool
    }
makeLenses ''Track

_Volume :: Lens' Track CFloat
_Volume f t = fmap (\g -> t { _trackGain = V2 (g * l / a) (g * r / a)}) $ f a where
    V2 l r = _trackGain t  
    a = (l + r) / 2

_Pan :: Lens' Track CFloat
_Pan f t = fmap (\p' -> t { _trackGain = if p' < 0.5
        then V2 1 (p' * 2) ^* max l r
        else V2 (2 - p' * 2) 1 }) $ f p where
    V2 l r = _trackGain t
    p | l < r = (1 - r / l) / 2
      | r > l = (2 - l / r) / 2
      | otherwise = undefined

data UIMode = Master | SelectPart (Maybe Int) | EditPart Int

data PlayMode = Play | Pause | Record

cataPlayMode :: a -> a -> a -> PlayMode -> a
cataPlayMode x _ _ Play = x
cataPlayMode _ x _ Pause = x
cataPlayMode _ _ x Record = x

data World = World
    { _tracks :: IM.IntMap Track
    , _endFlag :: Bool
    , _uiMode :: UIMode
    , _playMode :: Maybe (PlayMode, Int)
    , _clockDur :: Int
    , _clock :: Int
    , _currentBiquadParam :: BiquadParam
    , _masterFX :: Vein (StateT World IO) Wave Wave
    , deviceId :: Int
    }
makeLenses ''World

beat :: Traversal' World Int
beat = playMode . traversed . _2

_BPM :: Lens' World Double
_BPM = lens (((44100*60/4)/) . fromIntegral . _clockDur) (\w b -> w { _clockDur = floor $ 44100 * 60 / 4 / b })

stateToMVar :: MonadIO m => MVar s -> StateT s m a -> m a
stateToMVar mv m = do
    s <- liftIO $ takeMVar mv
    (a, s') <- runStateT m s
    liftIO $ putMVar mv s'
    return a