module PingPong.Simulation.Collision
  ( CollisionChecker
  , defaultCollisionChecker
  , verify
  , Snapshot
  , buildCollisionCheckerIO
  , CollisionDetectorIO
  , CollisionHandlerIO
  ) where

import PingPong.Model

import Data.Geometry hiding (init, head, zero, replicate)
import Data.List hiding (intersect)
import Data.Foldable
import Data.Ext

import Control.Lens

import Data.Geometry.Transformation

-- * Implementation of collision checking.


-- | A collision checker takes as input two time stamps, with for each time stamp
--   the location of a point and a segment.
type Snapshot = (Second, Pnt, Seg)


-- | A collision checker takes as input two snapshots.
--   Assuming linear motion between the two time stamps, the checker should test
--   if a collision between the point and the segment takes place.
--   If so, it should report the time of the collision, as well as location and
--   velocity of the point as a result of the collision.
type CollisionChecker = Snapshot
                     -> Snapshot
                     -> IO (Maybe (Second, Pnt, Vec))

-- | A Collision detector only detects if there is a collision, and if so, when.
type CollisionDetector   = Snapshot -> Snapshot ->     Maybe Second
type CollisionDetectorIO = Snapshot -> Snapshot -> IO (Maybe Second)

-- | A Collision handler handles a collision, provided the time of collision.
type CollisionHandler   = Snapshot -> Snapshot -> Second ->    (Pnt, Vec)
type CollisionHandlerIO = Snapshot -> Snapshot -> Second -> IO (Pnt, Vec)

buildCollisionChecker :: CollisionDetector -> CollisionHandler -> CollisionChecker
buildCollisionChecker detect handle snap1 snap2 =
  case detect snap1 snap2 of 
    Nothing -> return Nothing
    Just t  -> let (p, v) = handle snap1 snap2 t
               in return $ Just (t, p, v)

buildCollisionCheckerIO :: CollisionDetectorIO -> CollisionHandlerIO -> CollisionChecker
buildCollisionCheckerIO detect handle snap1 snap2 = do
  det <- detect snap1 snap2
  case det of
    Nothing -> return Nothing
    Just t  -> do (p, v) <- handle snap1 snap2 t
                  return $ Just (t, p, v)

-- The collision checker that will be used when running the simulator through the play function.
defaultCollisionChecker :: CollisionChecker
defaultCollisionChecker = floorChecker

-- Make sure a collision checker returns something with a sensible time stamp.
verify :: CollisionChecker -> CollisionChecker
verify checker st1 st2 = do
  result <- checker st1 st2
  return $ verifyResult st1 st2 result

verifyResult :: Snapshot
             -> Snapshot
             -> Maybe (Second, Pnt, Vec)
             -> Maybe (Second, Pnt, Vec)
verifyResult _ _ Nothing = Nothing
verifyResult (t1, p1, s1) (t2, p2, s2) (Just (t, p, v)) | t <= t1 = Nothing
                                                        | t >= t2 = Nothing
                                                        | otherwise = Just (t, p, v)

-- A simple collision checker which ignores the segment, and only checks for collision with the floor.
floorChecker :: CollisionChecker
floorChecker (t1, Point2 x1 y1, _) (t2, Point2 x2 y2, _)
  | y2 >= 0   = return Nothing
  | y1 == y2  = error "Ball was already under the floor!?"
  | otherwise = let tc = t1 + (t2 - t1) * y1 / (y1 - y2)
                    xc = x1 + (x2 - x1) * y1 / (y1 - y2)
                    yc = 0
                    dx = (x2 - x1) / (t2 - t1)
                    dy = (y1 - y2) / (t2 - t1)
                in return $ Just (tc, Point2 xc yc, Vector2 dx dy)
  
