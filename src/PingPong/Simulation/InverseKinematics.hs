module PingPong.Simulation.InverseKinematics where

-- implementation of inverse kinematics 

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
import Data.Fixed


import PingPong.Model
import PingPong.Model.AlmostEqual
import PingPong.Simulation.ForwardKinematics



inverse :: Arm -> Seg -> Maybe [Radian]
inverse arm seg = 
  let goal = seg ^. start . core
      msol = solve goal arm
  in case msol of Nothing  -> Nothing
                  Just sol -> let fang = finalAngle sol $ seg ^. end . core
                              in  Just $ jointAngles $ setFinalAngle fang arm

-- | Solve approximate inverse kinematics putting the final joint at the goal location, 
--   ignoring the orientation of the bat.
solve :: Pnt -> Arm -> Maybe Arm
solve pnt arm =
  let iterativeCyclicDescent :: Int -> Arm -> Maybe Arm
      iterativeCyclicDescent index arm | index >= numberOfJoints arm = iterativeCyclicDescent 1 arm
      iterativeCyclicDescent index arm = 
        let piv = evaluatePivot arm
            vec = pnt .-. piv
        in if norm vec < epsilon then Just arm
           else let updatedArm = updateArm index pnt arm
                in iterativeCyclicDescent (index + 1) updatedArm
                -- Should also check whether making enough progress.
  in iterativeCyclicDescent 1 arm

epsilon :: Float
epsilon = 0.0001

-- Update an arm by changing the ith joint angle to get the pivot to move closer to the desired point.
updateArm :: Int -> Pnt -> Arm -> Arm
updateArm i p a = 
  let piv = evaluatePivot a
      ith = evaluateArm a !! i
      cur = jointAngles a !! (i - 1)
      dif = angle (piv .-. ith) (p .-. ith)
      new = addRadian cur dif
  -- question: update immediately to local minimum or take a fraction of the change?
  in setJointAngle i new a    

evaluatePivot :: Arm -> Pnt
evaluatePivot arm = last $ init $ evaluateArm arm

-- | Compute exactly the angle needed for the final joint to reach in the
--   direction of the point.
finalAngle :: Arm -> Pnt -> Radian
finalAngle arm pnt = 
  let tip = last $        evaluateArm arm
      piv = last $ init $ evaluateArm arm
      cur = last $        jointAngles arm
      dif = angle (tip .-. piv) (pnt .-. piv)
  in addRadian cur dif

setFinalAngle :: Radian -> Arm -> Arm
setFinalAngle ang arm = setJointAngle (length $ jointAngles arm) ang arm

-- | Set the ith joint (counting from 1).
setJointAngle :: Int -> Radian -> Arm -> Arm
setJointAngle i r a | i > numberOfJoints a = error "setJointAngle: index too large"
setJointAngle i r a | i < 1                = error "setJointAngle: index too small"
setJointAngle 1 r (Extend l j a) = Extend l (j {jang = r}) a
setJointAngle i r (Extend l j a) = Extend l j $ setJointAngle (i - 1) r a
setJointAngle _ r (End l) = error "There is no angle to set."

numberOfJoints :: Arm -> Int
numberOfJoints = length . jointAngles

jointAngles :: Arm -> [Radian]
jointAngles (Extend l j a) = jang j : jointAngles a
jointAngles _ = []

angle :: (Arity d, Floating r) => Vector d r -> Vector d r -> r
angle u v = dot u v / norm u / norm v

addRadian :: Radian -> Radian -> Radian
addRadian a b = a + b `mod'` 2 * pi








-- Given an arm and a desired velocity vector for the pivot (first endpoint of the
-- bat), compute a list of angular velocities for the joints that achieves this..
inverseVelocity :: Arm -> Vec -> Motion
inverseVelocity arm goal = 
  let points  = tail $ init $ evaluateArm arm
      pivot   = last $ points
      vectors = map (computeVector pivot) points
      -- maybe we don't just want a solution, but one that is close to the current velocities?
      current = map jvel (armJoints arm)
      numberOfIterations = 100
  in iterativeLinearCombination numberOfIterations goal vectors current
{-
  in case findLinearCombination vectors goal
     of Just coefficients -> coefficients
        Nothing           -> current
-}

computeVector :: Pnt -> Pnt -> Vec 
computeVector q p = rotate90 $ q .-. p

-- findLinearCombination :: [Vec] -> Vec -> Maybe [Float]

-- Given a current set of coefficients and vectors, find a set of coefficients close
-- to the current ones that achieves the goal vector
iterativeLinearCombination :: Int -> Vec -> [Vec] -> [Float]-> [Float]
iterativeLinearCombination 0 goal vs fs = fs
iterativeLinearCombination n goal vs fs =
  let now = linCom fs vs
      dif = goal ^-^ now 
  in if dif ~= zero then fs else iterativeLinearCombination (n-1) goal vs $ updateCoefficients dif vs fs

updateCoefficients :: Vec -> [Vec] -> [Float] -> [Float]
updateCoefficients = zipWith . updateCoefficient

updateCoefficient :: Vec -> Vec -> Float -> Float
updateCoefficient dif v f = 
  let d = project dif v
      x = d - f
      fraction = 0.1
  in capSpeed $ f + fraction * x

linCom :: [Float] -> [Vec] -> Vec  
linCom fs vs = foldr (^+^) zero $ zipWith (*^) fs vs

project :: Vec -> Vec -> Float
project a b = a `dot` signorm b