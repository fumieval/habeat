{-# LANGUAGE LambdaCase, BangPatterns #-}
module Process (sampler, runTrack) where
import Types
import Vein
import Linear
import Data.WAVE

sampler :: FilePath -> IO (Vein IO Bool Wave)
sampler path = do
    WAVE _ s <- getWAVEFile path
    let !s' = mkSample s
    let ar rs = let (x, xs) = maybe (zero, []) id (uncons rs)
            in Vein $ \case
            True -> return $! (x, ar (go s' xs))
            False -> return $! (x, ar xs)
    return (ar [])    
    where
        go (x:xs) (y:ys) = x + y : go xs ys
        go xs [] = xs
        go [] ys = ys

        mkSample :: WAVESamples -> [Wave]
        mkSample [] = []
        mkSample ([l, r]: xs) = V2 (realToFrac (sampleToDouble l)) (realToFrac (sampleToDouble r)) : mkSample xs
        mkSample ([v]: xs) = V2 (realToFrac (sampleToDouble v)) (realToFrac (sampleToDouble v)) : mkSample xs 
        mkSample _ = error "illegal sample"

_dB :: Floating a => Iso' a a
_dB = iso ((*20) . logBase 10) ((10 **) . (/ 20))

runTrack :: StateT Track IO Wave
runTrack = do
    v <- use trackVein
    p <- use pending
    (w, v') <- lift $ runVein v (or p)
    pending .= []
    trackVein .= v'
    return w