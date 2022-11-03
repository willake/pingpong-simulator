module PingPong.Submission.ModelNativeSubmission (submission) where

import PingPong.Model hiding (name, arm, dance, plan, action)
import PingPong.Model.AlmostEqual
import PingPong.Player
import qualified PingPong.Submission as Submission

import PingPong.Simulation.Collision hiding (modelHandler, modelDetector)

import Control.Lens

import Data.Geometry hiding (head, init)
import Data.Geometry.Matrix
import Data.Ext
import Data.List hiding (head, init, intersect)
import Data.Colour
import Data.Colour.Names
import Data.Fixed


import Debug.Trace

-- Collect all exercises in a single record.
submission = Submission.Submission
  { Submission.name            = name
  , Submission.arm             = arm
  , Submission.detectCollision = detectCollision
  , Submission.handleCollision = handleCollision
  , Submission.controlArm      = controlArm
  , Submission.evaluateArm     = evaluateArm
  , Submission.dance           = dance
  , Submission.inverse         = inverse
  , Submission.plan            = plan
  , Submission.action          = action
  }


---------------------
-- FOR EXERCISE B1 --
---------------------

name :: String
name = "Cor du Buy"

arm  :: Arm
arm = link black 0.3 -* joint black (-0.7)
   *- link black 0.3 -* joint black ( 1.2)
   *- link black 0.3 -* joint black ( 1.0)
   *- link black 0.3 -* joint black (-0.5)
   *- bat  red

---------------------
-- FOR EXERCISE B2 --
---------------------


detectCollision :: Snapshot -> Snapshot -> Maybe Second
detectCollision = modelDetector

-- | A potential collision is given by two real numbers: a time fraction,
--   and a fraction of the segment.
type PotentialCollision = (Float, Float)

-- | A potential collision is valid when both fractions are in the interval [0,1].
validCollision :: PotentialCollision -> Bool
validCollision (a, b) = a >= 0 && a <= 1 && b >= 0 && b <= 1

-- | The time at which a potential collision happens.
collisionTime :: Snapshot -> Snapshot -> PotentialCollision -> Second
collisionTime (t1, _, _) (t2, _, _) (u, _) = (1 - u) * t1 + u * t2

-- | The model detector.
modelDetector :: Snapshot -> Snapshot -> Maybe Second
modelDetector snap1 snap2 =
  let pocos = filter validCollision $ potentialCollisions snap1 snap2
  in case pocos of []      -> Nothing
                   (t : _) -> Just $ collisionTime snap1 snap2 t

-- | Collect all potential collisions. May return 0, 1, or 2 pairs of fractions.
--   Note that the snapshot times are irrelevant.
potentialCollisions :: Snapshot -> Snapshot -> [PotentialCollision]
potentialCollisions (_, b0, s0) (_, b1, s1) = 
  let c0 = s0 ^. start ^. core
      d0 = s0 ^. end   ^. core
      c1 = s1 ^. start ^. core
      d1 = s1 ^. end   ^. core
      Point2 xb0 yb0 = b0
      Point2 xb1 yb1 = b1
      Point2 xc0 yc0 = c0
      Point2 xc1 yc1 = c1
      Point2 xd0 yd0 = d0
      Point2 xd1 yd1 = d1
      xa = xd0 - xc0
      ya = yd0 - yc0
      xb = xb0 - xc0 + xc1 - xb1
      yb = yb0 - yc0 + yc1 - yb1 
      xc = xc0 - xd0 + xd1 - xc1
      yc = yc0 - yd0 + yd1 - yc1
      xd = xb0 - xc0
      yd = yb0 - yc0
      i = xd * ya - yd * xa
      j = xd * yc - xb * ya - yd * xc + yb * xa
      k = yb * xc - xb * yc
      us = solveQuadraticEquation k j i
      s u | almostZero $ xa + xc * u = (yd - yb * u) / (ya + yc * u)
          | almostZero $ ya + yc * u = (xd - xb * u) / (xa + xc * u)
          | otherwise = let s1 = (xd - xb * u) / (xa + xc * u)
                            s2 = (yd - yb * u) / (ya + yc * u)
                        in if 0 <= s1 && s1 <= 1 then s1 else s2 -- checkAlmostEqual s1 s2 
  in sort $ zip us $ map s us

-- | Solve equation of the form ax^2 + bx + c = 0.
--   Attempt at a somewhat robust implementation.
solveQuadraticEquation :: (Ord r, Enum r, Floating r, Show r) => r -> r -> r -> [r]
solveQuadraticEquation 0 0 0 = [0] -- [0..]
solveQuadraticEquation a 0 0 = [0]
solveQuadraticEquation 0 b 0 = [0]
solveQuadraticEquation 0 0 c = []
solveQuadraticEquation a b 0 = sort [0, -b / a]
solveQuadraticEquation a 0 c | (-c / a) <  0 = []
                             | (-c / a) == 0 = [0]
                             | (-c / a) >  0 = [sqrt (-c / a)]
solveQuadraticEquation 0 b c = [-c / b]
solveQuadraticEquation a b c | almostZero a || almostZero (a / b) || almostZero (a / c) = solveQuadraticEquation 0 b c
solveQuadraticEquation a b c = 
  let d = b^2 - 4 * a * c
      result | d == 0 = [-b / (2 * a)]
             | d >  0 = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
             | otherwise = []
  in result

-- | Test whether a floating point number is zero, taking rounding errors into account.
almostZero :: (Floating r, Ord r) => r -> Bool
almostZero x = abs x < epsilon

-- | Treshold for rounding errors in zero tests.
epsilon :: Floating r => r
epsilon = 0.0001


---------------------
-- FOR EXERCISE B3 --
---------------------

controlArm :: Second -> Control -> Arm -> Arm
controlArm td c a = advanceArm td $ applyControl td c a

applyControl :: Second -> Control -> Arm -> Arm
applyControl td [] (End l) = End l
applyControl td (f : fs) (Extend l j a) = Extend l (controlJoint td f j) (applyControl td fs a)
applyControl td _ a = a -- wrong length of control vector

-- | Apply acceleration to a joint, taking into account maximum speed and acceleration.
controlJoint :: Second -> RadianPerSquareSecond -> Joint -> Joint
controlJoint td f (Joint c a v) = Joint c a (capSpeed (v + (capAcceleration f) * td))

advanceArm :: Second -> Arm -> Arm 
advanceArm td (End l)        = End l
advanceArm td (Extend l j a) = Extend l (advanceJoint td j) (advanceArm td a)

advanceJoint :: Second -> Joint -> Joint
advanceJoint td (Joint c a v) = Joint c (a + v * td) v





-- | Give the vertices of the arm in a list, including the base and the tip.
evaluateArm :: Arm -> [Point 2 Float]
evaluateArm arm = 
  let uniquePoints = nub $ evaluateArmElements arm
      points = case validateArmPoints arm uniquePoints of Just m  -> error $ "evaluateArm: " ++ m
                                                          Nothing -> uniquePoints
  in points             

validateArmPoints :: Arm -> [Pnt] -> Maybe String
validateArmPoints arm ps = 
  let links = map llen $ armLinks arm
      dists = map norm $ zipWith (.-.) (tail ps) (init ps)
  in if links ~= dists then Nothing
                       else Just $ show links ++ " is not the same as " ++ show dists



-- | Give the vertices of all arm elements. This will include duplicate vertices,
--   since the joint transformations are rotations and do not change the location.
evaluateArmElements :: Arm -> [Point 2 Float]
evaluateArmElements arm = map (origin .+^) 
                        $ map (flip transformBy $ Vector2 0 0) 
                        $ globalize $ transformations arm

transformations :: Arm -> [Transformation 2 Float]
transformations = map transformation . armElements

transformation :: Element -> Transformation 2 Float
transformation (Left  (Link  _ d  )) = translation $ Vector2 0 d
transformation (Right (Joint _ a _)) = rotation a

-- | Turns a sequence of local transformations into independent global transformations.
globalize :: [Transformation 2 Float] -> [Transformation 2 Float]
globalize ts = scanl (|.|) identity ts

dance :: Second -> Arm -> Control
dance t _ = [ -20 * sin (3 * t)
            ,  20 * cos (3 * t)
            , -20 * sin (3 * t)
            ,  20 * cos (3 * t)
            ]

---------------------
-- FOR EXERCISE B4 --
---------------------

handleCollision :: Snapshot -> Snapshot -> Second -> (Pnt, Vec)
handleCollision = modelHandler


modelHandler :: Snapshot -> Snapshot -> Second -> (Pnt, Vec)
modelHandler snap1 snap2 t = 
  let (p, s) = collisionPoint snap1 snap2 t
      v      = collisionVelocity snap1 snap2 t (p, s)
  in (p, v)


-- | For a given collision time, compute the corresponding point in space, and also 
--   report the fraction of the segment where the collision happens.
collisionPoint :: Snapshot -> Snapshot -> Second -> (Pnt, Float)
collisionPoint (t0, b0, seg0) (t1, b1, seg1) t =
  let c0 = seg0 ^. start ^. core
      d0 = seg0 ^. end   ^. core
      c1 = seg1 ^. start ^. core
      d1 = seg1 ^. end   ^. core
      Point2 xb0 yb0 = b0
      Point2 xb1 yb1 = b1
      Point2 xc0 yc0 = c0
      Point2 xc1 yc1 = c1
      Point2 xd0 yd0 = d0
      Point2 xd1 yd1 = d1
      xa = xd0 - xc0
      ya = yd0 - yc0
      xb = xb0 - xc0 + xc1 - xb1
      yb = yb0 - yc0 + yc1 - yb1 
      xc = xc0 - xd0 + xd1 - xc1
      yc = yc0 - yd0 + yd1 - yc1
      xd = xb0 - xc0
      yd = yb0 - yc0
      u = (t - t0) / (t1 - t0)
      p = origin .+^ (1-u) *^ (b0 .-. origin) 
                 .+^ u     *^ (b1 .-. origin)
      s | almostZero $ xa + xc * u = (yd - yb * u) / (ya + yc * u)
        | almostZero $ ya + yc * u = (xd - xb * u) / (xa + xc * u)
        | otherwise = let s1 = (xd - xb * u) / (xa + xc * u)
                          s2 = (yd - yb * u) / (ya + yc * u)
                      in if 0 <= s1 && s1 <= 1 then s1 else s2 -- checkAlmostEqual s1 s2 
  in (p, s)

checkAlmostEqual :: (Ord r, Floating r, Show r) => r -> r -> r
checkAlmostEqual a b | a == 0 && abs b > treshold = error message
                     | b == 0 && abs a > treshold = error message
                     | a == 0 || b == 0           = 0
                     | a / b > 1 + treshold       = error message
                     | b / a > 1 + treshold       = error message
                     | otherwise                  = a -- trace ("checking " ++ show a ++ " " ++ show b) a
  where treshold = 100
        message  = error $ "checkAlmostEqual: " ++ show a ++ " /= " ++ show b

-- | For a given collision time and corresponding point and fraction, compute the new velocity 
--   of the ball at that point and time.
collisionVelocity :: Snapshot -> Snapshot -> Second -> (Pnt, Float) -> Vec
collisionVelocity (t0, b0, seg0) (t1, b1, seg1) t (p, s) | t < t0 || t > t1 = error "collisionVelocity: time not in range"
collisionVelocity (t0, b0, seg0) (t1, b1, seg1) t (p, s) = 
  let u = (t - t0) / (t1 - t0)
      c0 = seg0 ^. start ^. core
      d0 = seg0 ^. end   ^. core
      c1 = seg1 ^. start ^. core
      d1 = seg1 ^. end   ^. core
      vl =   ((1-u) *^ (d0 .-. origin) ^+^ u *^ (d1 .-. origin)) 
         ^-^ ((1-u) *^ (c0 .-. origin) ^+^ u *^ (c1 .-. origin))
      vs =   (((1-s) *^ (c1 .-. origin) ^+^ s *^ (d1 .-. origin)) 
         ^-^ ((1-s) *^ (c0 .-. origin) ^+^ s *^ (d0 .-. origin)))
         ^/  (t1 - t0)
      vb = (b1 .-. b0) ^/ (t1 - t0)
      vr = vb ^-^ vs
      vm = reflectVector vr vl
  in vm ^+^ vs

-- | Reflect vector 'a' in a line with direction vector 'b'.
reflectVector :: Vector 2 Float -> Vector 2 Float -> Vector 2 Float
reflectVector a b = reflection (angle (Vector2 1 0) b) `transformBy` a

-- | Find the angle between two vectors, in counter-clockwise order, from the first to the second.
angle :: Vector 2 Float -> Vector 2 Float -> Float
angle (Vector2 x1 y1) (Vector2 x2 y2) = atan2 (x1 * y2 - y1 * x2) (x1 * x2 + y1 * y2)

-- A more general version of angle which is not correct?
angle' :: (Arity d, Floating r) => Vector d r -> Vector d r -> r
angle' u v = dot u v / norm u / norm v

---------------------
-- FOR EXERCISE B5 --
---------------------


inverse :: Arm -> Seg -> Maybe [Radian]
inverse arm seg = 
  let numberOfIterations = 1000
      (result1, ce1) = solveInverseKinematics numberOfIterations seg arm
      (result2, ce2) = solveInverseKinematics numberOfIterations (flipSegment seg) arm
  in if ce1 then Just $ jointAngles result1 else if ce2 then Just $ jointAngles result2 else Nothing

solveInverseKinematics :: Int -> Seg -> Arm -> (Arm, Bool)
solveInverseKinematics numberOfIterations seg arm =
  let goal      = seg ^. start . core
      (sol, ce) = solve numberOfIterations goal arm
      fang      = finalAngle sol $ seg ^. end . core
      result    = setFinalAngle fang sol
  in (result, ce)

-- | Solve approximate inverse kinematics putting the final joint at the goal location, 
--   ignoring the orientation of the bat.
solve :: Int -> Pnt -> Arm -> (Arm, Bool)
solve numberOfIterations pnt arm =
  let iterativeCyclicDescent :: Int -> Int -> Arm -> (Arm, Bool)
      iterativeCyclicDescent 0 _     arm = (arm, False)
      iterativeCyclicDescent n index arm | index >= numberOfJoints arm = iterativeCyclicDescent n 1 arm
      iterativeCyclicDescent n index arm = 
        let piv = evaluatePivot arm
            vec = pnt .-. piv
        in if norm vec < epsilon then (arm, True)
           else let updatedArm = updateArm index pnt arm
                in iterativeCyclicDescent (n - 1) (index + 1) updatedArm
                -- Would be better to also check whether making enough progress.
  in iterativeCyclicDescent numberOfIterations 1 arm

-- Update an arm by changing the ith joint angle to get the pivot to move closer to the desired point.
updateArm :: Int -> Pnt -> Arm -> Arm
updateArm i p a = 
  let piv = evaluatePivot a
      ith = evaluateArm a !! i
      cur = jointAngles a !! (i - 1)
      dif = angle (piv .-. ith) (p .-. ith)
      frc = 0.999 * dif -- move only a fraction to avoid local minima
      new = addRadian cur frc
  in setJointAngle i new a    

evaluatePivot :: Arm -> Pnt
evaluatePivot arm = last $ init $ evaluateArm arm

-- | Compute exactly the angle needed for the final joint to reach in the
--   direction of the given point.
finalAngle :: Arm -> Pnt -> Radian
finalAngle arm pnt = 
  let tip = last $        evaluateArm arm
      piv = last $ init $ evaluateArm arm
      cur = last $        jointAngles arm
      u   = (tip .-. piv)
      v   = (pnt .-. piv) 
      dif = angle u v
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


addRadian :: Radian -> Radian -> Radian
addRadian a b = (a + b) `mod'` (2 * pi)

---------------------
-- FOR EXERCISE B6 --
---------------------


-- Given an arm and a desired velocity vector for the pivot (first endpoint of the
-- bat), compute a list of angular velocities for the joints that achieves this..
inverseVelocity :: Arm -> Vec -> Motion
inverseVelocity arm goal = 
  let points  = tail $ init $ evaluateArm arm
      pivot   = last $ points
      vectors = map (computeVector pivot) points
      -- maybe we don't just want a solution, but one that is close to the current velocities?
      current = map jvel (armJoints arm)
      numberOfIterations = 10
      speeds  = iterativeLinearCombination numberOfIterations goal vectors current
  in updateFinalSpeed speeds
{-
  in case findLinearCombination vectors goal
     of Just coefficients -> coefficients
        Nothing           -> current
-}

-- Set the speed of the last joint such that the last segment keeps its orientation.
updateFinalSpeed :: [RadianPerSecond] -> [RadianPerSecond]
updateFinalSpeed xs = let ys = init xs in ys ++ [negate $ sum ys]

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
updateCoefficient dif v f | v ~= zero = f
updateCoefficient dif v f = 
  let d = project dif v
      x = d - f
      fraction = 0.3
  in capSpeed $ f + fraction * x

linCom :: [Float] -> [Vec] -> Vec  
linCom fs vs = foldr (^+^) zero $ zipWith (*^) fs vs

project :: Vec -> Vec -> Float
project a b = a `dot` signorm b




plan :: Second -> Arm -> Second -> Seg -> Vec -> Control
plan t0 arm t1 seg vec =
  let (goalArm, close) = solveInverseKinematics 10 seg arm
      goalMotion       = inverseVelocity goalArm vec
      realGoalArm      = setJointVelocities goalMotion goalArm 
      js = armJoints $ arm
      gs = armJoints $ realGoalArm
  in zipWith (planJoint t0 t1) js gs
     -- planJoint t0 t1 (head js) (head gs) : []

setJointAngles :: [Radian] -> Arm -> Arm
setJointAngles (r : rs) (Extend l j a) = Extend l (j {jang = r}) $ setJointAngles rs a
setJointAngles []       (Extend l j a) = error "setJointAngles: Not enough angles."
setJointAngles []       (End l)        = End l
setJointAngles (r : rs) (End l)        = error "setJointAngles: Too many angles."

setJointVelocities :: [RadianPerSecond] -> Arm -> Arm
setJointVelocities (r : rs) (Extend l j a) = Extend l (j {jvel = r}) $ setJointVelocities rs a
setJointVelocities []       (Extend l j a) = error "setJointVelocities: Not enough velocities."
setJointVelocities []       (End l)        = End l
setJointVelocities (r : rs) (End l)        = error "setJointVelocities: Too many velocities."

modAngle :: Float -> Float
modAngle x = let y = ((x + pi) `mod'` (2 * pi)) - pi
             in -- trace ("modAngle " ++ show x ++ " = " ++ show y) y
                y

rotate90 :: Vector 2 Float -> Vector 2 Float
rotate90 (Vector2 x y) = Vector2 (-y) x

planJoint :: Second -> Second -> Joint -> Joint -> RadianPerSquareSecond
planJoint t0 t1 (Joint _ u0 v0) (Joint _ u1 v1) = planMotion (t0, modAngle u0, v0) (t1, modAngle u1, v1)


planMotion :: DataPoint -> DataPoint -> RadianPerSquareSecond
planMotion (t0, u0, v0) (t1, u1, v1) = 
  let Vector4 a b c d = fitCubic (0, u0, v0) (t1 - t0, u1, v1)
  in 2 * b

{-
f(t0) = u0
f'(t0) = v0
f(t1) = v1
f'(t1) = v1

f(x) = a x^3 + b x^2 + c x + d
f'(x) = 3a x^2 + 2b x + c
f''(x) = 6a x + 2b

a t0^3 + b t0^2 + c t0 + d = u0
a t1^3 + b t1^2 + c t1 + d = u1
3a t0^2 + 2b t0 + c = v0
3a t1^2 + 2b t1 + c = v1

-}

type DataPoint = (Second, Radian, RadianPerSecond)

-- Fit a cubic polynomial, returned as a vector of 4 coefficients.
fitCubic :: DataPoint -> DataPoint -> Vector 4 Float
fitCubic (t0, u0, v0) (t1, u1, v1) =
  let m = Matrix ( Vector4 (Vector4 (    t0 ** 3) (    t0 ** 2) t0 1 )
                           (Vector4 (    t1 ** 3) (    t1 ** 2) t1 1 )
                           (Vector4 (3 * t0 ** 2) (2 * t0     )  1 0 )
                           (Vector4 (3 * t1 ** 2) (2 * t1     )  1 0 )
                 )
      r = Vector4 u0 u1 v0 v1
  in solveLinearSystem m r

solveLinearSystem :: Matrix 4 4 Float -> Vector 4 Float -> Vector 4 Float
solveLinearSystem m v  | det m /= 0 = inverse' m `mult` v
                       | otherwise  = Vector4 0 0 0 0 -- is there something more reasonable here?
-- = inverse' m `mult` v

---------------------
-- FOR EXERCISE B7 --
---------------------

{-

action :: Second -> Item -> Arm -> BallState -> Control
action time item arm ball = plan time arm goalTime goalSeg goalVec
  where
    goalTime = 8.7
    goalSeg  = OpenLineSegment (Point2 (-0.3) 0.7 :+ ()) (Point2 (-0.3) 0.8 :+ ())
    goalVec  = Vector2 (-1) 0
  -- first, find a suitable goal position
  -- then, actually go there

-}



-- if last thing hit is not table: get the time and location when the all hits the table on our side
-- predict resulting ball state

-- determine a good location to intercept the ball

-- only when close to the ball (?): decide on speed and angle to return the ball


action :: Second -> Item -> Arm -> BallState -> Control
action time item arm ball = 
  let foot     = 1.5
      goalX    = 1.2
      (t, h)   = interceptBall goalX ball
      goalTime = time + t
      goalSeg  = OpenLineSegment (Point2 (goalX - foot) (h - 0.05) :+ ()) (Point2 (goalX - foot) (h + 0.05) :+ ())
      goalVec  = Vector2 (-1) 0
  in plan time arm goalTime goalSeg goalVec


-- Very rudimentary implementation
interceptBall :: Meter -> BallState -> (Second, Meter)
interceptBall x (BallState (Point2 bx by) (Vector2 vx vy))
  | bx < x && vx <= 0 = (1, by)
  | bx > x && vx >= 0 = (1, by)
  | otherwise         = let t = (x - bx) / vx
                            h = by + t * vy - t ** 2
                            h' | h < 0.5   = 1 - h
                               | otherwise = h
                        in (t, h')

{-
gravity :: MeterPerSquareSecond
gravity = 2

f (x) = a x^2 + b x + c

f'' (x) = -2 = 2a => a = -1

f' (x) = 2a x + b = -2x + b
f' (0) = b = dy

f (x) = - x^2 + dy x + y
-}