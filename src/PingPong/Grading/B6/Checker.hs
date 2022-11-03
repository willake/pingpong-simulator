module PingPong.Grading.B6.Checker (getTestCaseRefs, checkSubmission, testPlan) where

import PingPong.Grading.B6.Types
import PingPong.Grading.Types

import PingPong.Model hiding (score)
import PingPong.Model.AlmostEqual

import PingPong.Player

import PingPong.Submission hiding (arm, score, evaluateArm, controlArm, catchErrorCall, catchInterfaceException, catchExceptions)
import PingPong.Simulation.Collision
import PingPong.Simulation.ForwardKinematics
import PingPong.Communication.Types
import PingPong.Communication.Interface
import PingPong.Draw
import PingPong.Draw.Rasterific

import qualified PingPong.Submission.ModelNativeSubmission as ModelNativeSubmission

import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Aeson

import Control.Monad
import Control.Lens
import Control.Exception

import System.Directory
import System.Random
import Graphics.Rasterific
import Graphics.Text.TrueType( Font, loadFontFile )

-- | Produce a list of all test cases to be used, and their corresponding point values
getTestCaseRefs :: IO [TestCaseRef]
getTestCaseRefs = do
  let dir = fileDir assignment
  files <- listDirectory dir
  cases <- mapM readTestCase $ map (dir ++) files
  return $ sort $ map (view _1) cases
  

getTestCase :: TestCaseRef -> IO TestCase
getTestCase ref = readTestCase $ filePath (asid ref) (tcid ref)

readTestCase :: FilePath -> IO TestCase
readTestCase path = do
  x <- B.readFile path
  let ecase = eitherDecode x
  case ecase of Left  m -> error $ "readTestCase failed: " ++ show m
                Right c -> return c

-- | Check a given submission against a single test case
checkSubmission :: IOSubmission -> TestCaseRef -> IO TestResult
checkSubmission sub ref = do
  player <- makePlayer sub
  let planner  = planIO     sub
      evaluator = evaluateArmIO sub -- replace by model evaluator for grading
  testcase@(_, i) <- getTestCase ref
  testPlan player planner testcase

framerate = 50.0
step :: Second
step = 1 / framerate



executePlan :: (Second -> Arm -> Second -> Seg -> Vec -> IO Control) -> (Second, Arm, Second, Seg, Vec) -> IO [(Second, Arm)]
executePlan planner (t1, arm, t2, seg, vec) | t1 > t2 + 1  = return []
                                            | t1 > t2      = do
  let newArm = advanceArmRaw step arm  
  recurse <- executePlan planner (t1 + step, newArm, t2, seg, vec)
  return $ (t1, arm) : recurse                                              
                                            | otherwise    = do
  control <- planner t1 arm t2 seg vec
  let newArm = controlArm step control arm  
  recurse <- executePlan planner (t1 + step, newArm, t2, seg, vec)
  return $ (t1, arm) : recurse

armMotion :: Arm -> Motion
armMotion = map jvel . armJoints

evaluateSnap :: Seg -> Vec -> (Second, Arm) -> (Second, Meter, Meter)
evaluateSnap seg vec (time, arm) = 
  let pts = evaluateArm arm
      p   = last $ init pts
      q   = last pts
      sps = motionVelocity (armMotion arm) arm
      v   = 0.5 *^ (last $ init sps) ^+^ 0.5 *^ (last sps)
      de  = maximum [ norm (p .-. seg ^. start ^. core)
                    , norm (q .-. seg ^. end   ^. core)
                    ]
      ve  = norm $ vec ^-^ v
  in (time, de, ve)


type Curve = [(Float, Float)]

-- scoring for time
timeCurve :: Curve
timeCurve = [ (0  ,  2.5)
            , (0.2,  2  )
            , (1  ,  1  )
            , (5  ,  0  )
            , (100, -1  )
            ]

-- scoring for distance
distCurve :: Curve
distCurve = [ (0    ,  2.5)
            , (0.025,  2  )
            , (0.05 ,  1  )
            , (0.1  ,  0  )
            , (100  , -1  )
            ]

-- scoring for velocity
veloCurve :: Curve
veloCurve = [ (0    ,  2.5)
            , (0.2  ,  2  )
            , (0.4  ,  1  )
            , (0.8  ,  0  )
            , (100  , -1  )
            ]

scoreCurve :: Curve -> Float -> Float
scoreCurve ((a, b) : (c, d) : curve) x | x <= a           = b
                                       | a <= x && x <= c = b + (d - b) * (x - a) / (c - a)
                                       | x >= c           = scoreCurve ((c, d) : curve) x
scoreCurve [(a, b)] x = b
scoreCurve [] x = error "scoreCurve: empty curve"

scoreContinuous :: Second -> (Second, Meter, MeterPerSecond) -> Float
scoreContinuous g (t, d, v) =
  let st = scoreCurve timeCurve $ abs (g - t)
      sd = scoreCurve distCurve $ d
      sv = scoreCurve veloCurve $ v
  in st + sd + sv

scoreSnap :: Second -> (Second, Meter, MeterPerSecond) -> Int
scoreSnap g (t, d, v) = minimum [5, floor $ scoreContinuous g (t, d, v)]

writeSeg :: Seg -> String
writeSeg s = writePnt (s ^. start . core) ++ "---" ++ writePnt (s ^. end . core)

writePnt :: Pnt -> String
writePnt (Point2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

writeVec :: Vec -> String
writeVec (Vector2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

writeSnap :: (Second, Arm) -> String
writeSnap (t, a) = 
  let pts = evaluateArm a
      p   = last $ init pts
      q   = last pts
      sps = motionVelocity (armMotion a) a
      v   = 0.5 *^ (last $ init sps) ^+^ 0.5 *^ (last sps)
  in "at time " ++ show t ++ " the bat was at " ++ writeSeg (OpenLineSegment (p :+ ()) (q :+ ()))
                          ++ " with velocity " ++ writeVec v  

testPlan :: Player -> (Second -> Arm -> Second -> Seg -> Vec -> IO Control) -> TestCase -> IO TestResult
testPlan player planner (ref, (t1, arm, t2, seg, vec)) = catchExceptions $ do
  snaps <- executePlan planner (t1, arm, t2, seg, vec)
  let f snap = scoreContinuous t2 $ evaluateSnap seg vec snap
      g snap = scoreSnap t2 $ evaluateSnap seg vec snap
      best   = maximumBy (\x y -> compare (f x) (f y)) snaps
  pics <- recordPlan (tcid ref) player (t2, seg, vec) f best snaps
  let result = testResult { trid    = tcid ref
                          , success = True
                          , score   = g best
                          , message = "closest match: " ++ writeSnap best
                          , recording = pics
                          }              
  return result            






recordPlan :: Int -> Player -> (Second, Seg, Vec) -> ((Second, Arm) -> Float) -> (Second, Arm) -> [(Second, Arm)] -> IO [Pic]
recordPlan i player (gt, seg, vec) scoreFunction best snaps = do
  Right font <- loadFontFile "fonts/lato/Lato-Bold.ttf" 
  pics <- sequence $ map (recordSnap font i player (gt, seg, vec) scoreFunction best) snaps
  return pics


recordSnap :: Font -> Int -> Player -> (Second, Seg, Vec) -> ((Second, Arm) -> Float) -> (Second, Arm) -> (Second, Arm) -> IO Pic
recordSnap font i player (goalTime, seg, vec) scoreFunction (bestTime, bestArm) (t, a) = do
  let score = minimum [5, scoreFunction (t, a)]
      state = defState 
                   { p1    = player {arm = a, initArm = a}
                   , p2    = noPlayer
                   , phase = DuringRally
                   , time  = t
                   }
      sub = nativeToIOSubmission ModelNativeSubmission.submission
  armPic <- drawState (evaluateArmIO sub) font state
  pts <- evaluateArmIO sub bestArm
  let arr = segment (midPoint seg) (midPoint seg .+^ vec)
      bestSeg = segment (last $ init pts) (last pts)
      bestVel = armVelocity bestArm
      bestArr = segment (midPoint bestSeg) (midPoint $ segment (last (init pts) .+^ last (init bestVel)) (last (pts) .+^ last (bestVel)))
      pic = do armPic
               withColor (convertColor cGreen) $ drawSegment $ center $ playerTransform 1.5 True $ seg
               withColor (convertColor cGreen) $ drawArrow arrowSize $ center $ playerTransform 1.5 True $ arr
               if t < bestTime then return () else do withColor (convertColor cRed) $ drawSegment $ center $ playerTransform 1.5 True $ bestSeg
                                                      withColor (convertColor cRed) $ drawArrow arrowSize $ center $ playerTransform 1.5 True $ bestArr
               drawText font (convertColor cTable) 36 300 300 $ "(test plan " ++ show i ++ ")"
               drawText font (convertColor cTable) 12 300 400 $ "time = " ++ show t ++ ""
               drawText font (convertColor cTable) 12 300 450 $ "goal time = " ++ show goalTime ++ ""
               drawText font (convertColor cTable) 12 300 500 $ "score = " ++ show score ++ ""
--               withColor (convertColor cRed) $ fill $ circle (V2 (100 + 50 * t) (800 - 100 * score)) 4
  return pic

arrowSize = 4

midPoint :: Seg -> Pnt
midPoint seg = origin .+^ 0.5 *^ (seg ^. start . core .-. origin) 
                      .+^ 0.5 *^ (seg ^. end   . core .-. origin) 


segment :: Pnt -> Pnt -> Seg
segment p q = OpenLineSegment (p :+ ()) (q :+ ())

catchExceptions :: IO TestResult -> IO TestResult
catchExceptions = handle catchErrorCall
                . handle catchInterfaceException

catchInterfaceException :: InterfaceException -> IO TestResult
catchInterfaceException e = return $ failure { message = show e }

catchErrorCall :: ErrorCall -> IO TestResult
catchErrorCall e = return $ failure { message = show e }
