{-# LANGUAGE Rank2Types #-}
module Vein.Filter where

import Control.Vein
import Types
import Linear

data FilterParam = FilterParam
    { _filterCutOff :: CFloat
    , _filterQ :: CFloat
    }

mkFilter :: (forall r. CFloat -> CFloat -> (CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> r) -> r)
    -> Vein m (Wave, FilterParam) Wave
mkFilter f = go 0 0 0 0 where
    go x' x'' y' y'' = Vein $ \(x, FilterParam freq q) cont -> let omega = 2 * pi / 44100 * freq
        in f omega (sin omega / q) $ \a0 a1 a2 b0 b1 b2 ->
            let y = (b0 *^ x + b1 *^ x' + b2 *^ x'' - a1 *^ y' - a2 *^ y'') ^* (1/a0)
                in cont y (go x x' y y')

lowPass :: Vein m (Wave, FilterParam) Wave
lowPass = mkFilter $ \w a r -> let v = 1 - cos w in r (1 + a) (-2 * cos w) (1 - a) (v / 2) v (v / 2)

highPass :: Vein m (Wave, FilterParam) Wave
highPass = mkFilter $ \w a r -> let v = 1 + cos w in r (1 + a) (-2 * cos w) (1 - a) (v / 2) v (v / 2)

bandPass :: Vein m (Wave, FilterParam) Wave
bandPass = mkFilter $ \w a r -> r (1 + a) (-2 * cos w) (1 - a) (sin w / 2) 0 (sin w / 2)

notch :: Vein m (Wave, FilterParam) Wave
notch = mkFilter $ \w a r -> r (1 + a) (-2 * cos w) (1 - a) 1 (-2 * cos w) 1

allPass :: Vein m (Wave, FilterParam) Wave
allPass = mkFilter $ \w a r -> r (1 + a) (-2 * cos w) (1 - a) (1 - a) (-2 * cos w) (1 + a)

peaking :: CFloat -> Vein m (Wave, FilterParam) Wave
peaking gain = mkFilter $ \w a r -> r (1 + a * g) (-2 * cos w) (1 - a * g)
    (1 + a * g) (-2 * cos w) (1 - a * g) where g = sqrt $ 10 ** (gain / 20)
