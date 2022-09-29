module PingPong where

-- import           PingPong.Simulation.Recording

import Data.Aeson
import PingPong.Communication.JSON
import PingPong.Communication.Socket
import PingPong.Grading
import PingPong.Model
import PingPong.Player
import PingPong.Simulation.Realtime
import PingPong.Submission
import qualified PingPong.Submission.ExampleNativeSubmission as ExampleNativeSubmission

-- If your player is implemented in Haskell use the following:
{-
main :: IO ()
main = do

  -- Change this to your own submission.
  let submission = nativeToIOSubmission ExampleNativeSubmission.submission

  p1 <- makePlayer submission
  p2 <- makePlayer $ nativeToIOSubmission $ ExampleNativeSubmission.submission

-- If you want to test the collision and control behaviour of your submission in the simulator:
  playWithSubmission submission p1 p2

-- If you want to test the automatic grader on your submission:
--  gradeSubmission "B1" submission

  return ()
-}

-- If your player is implemented in Python use the following:
main :: IO ()
main = do
  -- Change this to your own submission.
  let submission = readSocketSubmission 6174 "Batman"

  p1 <- makePlayer submission
  p2 <- makePlayer $ nativeToIOSubmission $ ExampleNativeSubmission.submission

  -- If you want to test the collision and control behaviour of your submission in the simulator:
  -- playWithSubmission submission p1 p2

  -- If you want to test the automatic grader on your submission:
  -- gradeSubmission "B1" submission
  gradeSubmission "B2" submission

  return ()
