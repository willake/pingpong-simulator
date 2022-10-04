module PingPong.Submission where

-- | This module specifies a full submission of all practical exercises.

import PingPong.Model hiding (prepare, name, arm, terminate, dance, action)
import qualified PingPong.Model as Player
import PingPong.Player
import PingPong.Simulation.Collision

import Data.Composition

-- Native submission type, which collects all functions that students should implement.
data Submission = Submission
  { -- B1
    name            :: String
  , arm             :: Arm
    -- B2
  , detectCollision :: Snapshot -> Snapshot -> Maybe Second
    -- B3
  , controlArm      :: Second -> Control -> Arm -> Arm
  , evaluateArm     :: Arm -> [Pnt]
  , dance           :: Second -> Arm -> Control
    -- B4
  , handleCollision :: Snapshot -> Snapshot -> Second -> (Pnt, Vec)
    -- B5
  , inverse         :: Arm -> Seg -> (Maybe [Radian])
    -- B6
  , plan            :: Second -> Arm -> Second -> Seg -> Vec -> Control
    -- B7
  , action          :: Second -> Item -> Arm -> BallState -> Control
  }

-- IO version, to support submissions in foreign languages
data IOSubmission = IOSubmission
  { -- adminstrative stuff
    isNative          :: Bool
    -- B1
  , prepare           :: IO (String, Arm)
    -- B2
  , detectCollisionIO :: Snapshot -> Snapshot -> IO (Maybe Second)
    -- B3
  , controlArmIO      :: Second -> Control -> Arm -> IO Arm
  , evaluateArmIO     :: Arm -> IO [Pnt]
  , danceIO           :: Second -> Arm -> IO Control
    -- B4
  , handleCollisionIO :: Snapshot -> Snapshot -> Second -> IO (Pnt, Vec)
    -- B5
  , inverseIO         :: Arm -> Seg -> IO (Maybe [Radian])
    -- B6
  , planIO            :: Second -> Arm -> Second -> Seg -> Vec -> IO Control
    -- B7
  , actionIO          :: Second -> Item -> Arm -> BallState -> IO Control
  }

nativeToIOSubmission :: Submission -> IOSubmission
nativeToIOSubmission sub = IOSubmission
  { isNative          = True
  , prepare           = return (name sub, arm sub)
  , detectCollisionIO = compose2 return $ detectCollision sub
  , handleCollisionIO = compose3 return $ handleCollision sub
  , controlArmIO      = compose3 return $ controlArm sub
  , evaluateArmIO     = compose1 return $ evaluateArm sub
  , danceIO           = compose2 return $ dance sub
  , inverseIO         = compose2 return $ inverse sub
  , planIO            = compose5 return $ plan sub
  , actionIO          = compose4 return $ action sub
  }

-- | Given a submission, create a player. If this is a python submission,
--   calling this function will actually spawn a python process.
makePlayer :: IOSubmission -> IO Player
makePlayer sub = do
  (n, a) <- prepare sub
  return Player 
    { Player.name      = n
    , Player.arm       = a
    , Player.initArm   = a
    , Player.foot      = 1.5
    , Player.prepare   = return () -- should not be needed anymore...?
    , Player.dance     = danceIO sub
    , Player.stretch   = defaultStretch
    , Player.action    = \s (_, i) b a -> actionIO sub s i a b
    }
