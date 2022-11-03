module PingPong.Grading.B2.Checker (getTestCaseRefs, checkSubmission, validateTestCases) where

import PingPong.Grading.B2.Types

import PingPong.Model hiding (score)
import PingPong.Model.AlmostEqual
import PingPong.Grading.Types
import PingPong.Submission hiding (catchErrorCall, catchInterfaceException, catchExceptions)
import PingPong.Communication.Types
import PingPong.Communication.Interface

import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Control.Monad
import Control.Lens

import System.Directory
import System.Random
import Control.Exception



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
  let detector = detectCollisionIO sub
  testcase <- getTestCase ref
  testCollision detector testcase

testCollision :: ((Second, Pnt, Seg) -> (Second, Pnt, Seg) -> IO (Maybe Second)) -> TestCase -> IO TestResult
testCollision detector (ref, (state1, state2), correctAnswer) = catchExceptions $ do
  givenAnswer <- detector state1 state2
  putStrLn $ show givenAnswer
  let correct = correctAnswer ~= givenAnswer
      sco | correct   = value ref
          | otherwise = 0
      mes | correct   = "correct result: " ++ writeCollisionOutput givenAnswer
          | otherwise = "incorrect result: " ++ writeCollisionOutput givenAnswer ++ " should have been " ++ writeCollisionOutput correctAnswer
      result = testResult { trid    = tcid ref
                          , success = True
                          , score   = sco
                          , message = mes
                          }
  return result

catchExceptions :: IO TestResult -> IO TestResult
catchExceptions = handle catchErrorCall
                . handle catchInterfaceException

catchInterfaceException :: InterfaceException -> IO TestResult
catchInterfaceException e = return $ failure { message = show e }

catchErrorCall :: ErrorCall -> IO TestResult
catchErrorCall e = return $ failure { message = show e }

writeCollisionOutput :: TestOutput -> String
writeCollisionOutput Nothing = "no collision"
writeCollisionOutput (Just t) = "collision at time " ++ show t

-- validate test case by performing forward interpolation
-- should call this during generation process

validateTestCases :: IO ()
validateTestCases = do
  refs <- getTestCaseRefs
  bs <- sequence $ map validateTestCase refs
  putStrLn $ "number of valid cases: " ++ show (length $ filter id bs)

validateTestCase :: TestCaseRef -> IO Bool
validateTestCase ref = do
  (_, input, output) <- getTestCase ref
  putStr $ "validating test case " ++ show (tcid ref) ++ ": "
  let valid = case output of Just t  -> validateTrueCase input t
                             Nothing -> validateFalseCase input
  putStrLn $ show valid
  return valid

validateTrueCase :: TestInput -> Second -> Bool
validateTrueCase ((t1, p1, s1), (t2, p2, s2)) t | t  <= t1  = False
                                                | t  >= t2  = False
                                                | otherwise =
  let u = (t - t1) / (t2 - t1)
      p = interpolate' u p1 p2
      a = interpolate' u (s1 ^. start . core) (s2 ^. start . core)
      b = interpolate' u (s1 ^. end   . core) (s2 ^. end   . core)
      sd = segmentDistance p a b
--      (sd, _) = sqDistanceToSegArg p (OpenLineSegment (a :+ ()) (b :+ ())) 
      -- this is weird! distance to line and distance to segment are not the same
  in sd < 0.0001

segmentDistance :: Pnt -> Pnt -> Pnt -> Float
segmentDistance p a b = 
  let dl = sqrt $ sqDistanceTo p (lineThrough a b) 
      dx = intervalDistance (p ^. xCoord) (a ^. xCoord) (b ^. xCoord)
      dy = intervalDistance (p ^. yCoord) (a ^. yCoord) (b ^. yCoord)
  in maximum [dl, dx, dy]

intervalDistance :: Float -> Float -> Float -> Float
intervalDistance x a b | a > b            = intervalDistance x b a
                       | x < a            = a - x
                       | a <= x && x <= b = 0
                       | x > b            = b - x

interpolate' :: Float -> Pnt -> Pnt -> Pnt 
interpolate' u p q = origin .+^ (u *^ (q .-. origin) ^+^ (1 - u) *^ (p .-. origin))

validateFalseCase :: TestInput -> Bool
validateFalseCase _ = True -- how to validate this?