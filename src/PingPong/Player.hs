module PingPong.Player where

import PingPong.Model
import Data.Geometry
import Control.Lens
import Data.Char
import Data.Colour
import Data.Ext


import Data.Default
import Control.Exception



defaultPlayer :: Player
defaultPlayer = Player
  { name    = "DefaultPlayer"
  , arm     = link black 1 -* joint black 0 *- bat black
  , initArm = link black 1 -* joint black 0 *- bat black
  , foot    = 1.5
  , prepare = return ()
  , terminate = return ()
  , action  = const $ const $ const $ const $ return [0]
  , stretch = defaultStretch
  , dance   = defaultDance
  } 

instance Default Player where
  def = defaultPlayer

noPlayer :: Player
noPlayer = defaultPlayer
  { name = "nobody"
  , arm = End $ Link black 0.1
  }


-- | Default stretching behaviour.
defaultStretch :: Float -> Arm -> IO Control
defaultStretch t _ = return $ propcap $ map (* cos t) [-2, 6, -6, 6, -6]

-- | Default dancing behaviour.
defaultDance :: Float -> Arm -> IO Control
defaultDance t _ = return [ 5 * sin (2.5 * (t + 0.0))
                          , 5 * sin (2.5 * (t + 0.3))
                          , 5 * sin (2.5 * (t + 0.6))
                          , 5 * sin (2.5 * (t + 0.9))
                          , 5 * sin (2.5 * (t + 1.2))
                          ]

propcap :: [Float] -> [Float]
propcap xs | m < 2 = xs
           | otherwise = map (\x -> 2 * x / m) xs
  where m = maximum $ map abs xs         


