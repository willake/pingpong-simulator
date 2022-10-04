module PingPong.Simulation.ForwardKinematics where

-- implementation of forward kinematics	

import Data.Geometry hiding (init, zero)
import Data.Geometry.Matrix
import Data.Geometry.Transformation
import Data.Geometry.PolyLine
import Data.Geometry.Polygon

import Data.Ext
import Data.List hiding (filter, map, null)
import Data.Either
import Control.Lens

import Data.Colour


import PingPong.Model


-- applying control and advancing time

type Controller = Second -> Control -> Arm -> IO Arm

-- | Applies control vector to the joint velocities, 
--   and in turn applies joint velocities to joint angles.
controlArm :: Second -> Control -> Arm -> Arm
--controlArm td c a = advanceArmRaw td $ applyControl td c a
controlArm = error "controlArm not implemented" 


-- | Applies control vector to the joint velocities.
applyControl :: Second -> Control -> Arm -> Arm
applyControl td [] (End l) = End l
applyControl td (f : fs) (Extend l j a) = Extend l (controlJoint td f j) (applyControl td fs a)
applyControl td _ a = a -- wrong length of control vector

-- | Apply acceleration to a joint, taking into account maximum speed and acceleration.
controlJoint :: Second -> RadianPerSquareSecond -> Joint -> Joint
controlJoint td f (Joint c a v) = Joint c a (capSpeed (v + (capAcceleration f) * td))

-- | Apply acceleration to a joint, not taking into account maximum speed and acceleration.
controlJointRaw :: Second -> RadianPerSquareSecond -> Joint -> Joint
controlJointRaw td f (Joint c a v) = Joint c a (v + f * td)


-- | Advances the arm state by a given amount of time, not taking into account any phyisical obstacles.
advanceArmRaw :: Second -> Arm -> Arm 
advanceArmRaw td (End l)        = End l
advanceArmRaw td (Extend l j a) = Extend l (advanceJointRaw td j) (advanceArmRaw td a)

advanceJointRaw :: Second -> Joint -> Joint
advanceJointRaw td (Joint c a v) = Joint c (a + v * td) v




-- location of arm in space




type Evaluator = Arm -> IO [Point 2 Float]

-- | Give the vertices of the arm in a list, including the base and the tip.
evaluateArm :: Arm -> [Point 2 Float]
--evaluateArm = nub . evaluateArmElements
evaluateArm = error "evaluateArm not implemented" 



-- | Convert the arm directly to a polyline.
evaluateP :: Evaluator -> Arm -> IO (PolyLine 2 () Float)
evaluateP ev arm = do
  pts <- ev arm 
  return $ fromPointsUnsafe $ map (:+ ()) pts



-- | Places the arm in space. Should this be here?
armSegments :: Evaluator -> Player -> Bool -> IO [LineSegment 2 () Float]
armSegments ev p f = do
  rps <- ev $ arm p
  let trl = translation (Vector2 (foot p) 0)
      scl = scaling (Vector2 (if f then 1 else -1) 1)
      nps = map (transformBy (scl |.| trl)) rps
      fps = map (:+ ()) nps
  return $ zipWith (OpenLineSegment) (init fps) (tail fps)


-- Given a set of angular speeds, compute the resulting speeds of all joints.
motionVelocity :: Motion -> Arm -> [Vector 2 Float]
motionVelocity fs arm = computeVelocity $ zip (evaluateArm arm) ([0] ++ fs ++ [0])

computeVelocity :: [(Point 2 Float, Float)] -> [Vector 2 Float]
computeVelocity [] = []
computeVelocity ((p, a) : pas) = 
  let pvel = (zero :) $  map (\q -> (a *^) $ rotate90 $ q .-. p) $ map fst pas
      rvel = zero : computeVelocity pas
  in zipWith (^+^) pvel rvel

zero :: Vector 2 Float
zero = toVec origin

rotate90 :: Vector 2 Float -> Vector 2 Float
rotate90 (Vector2 x y) = Vector2 (-y) x
