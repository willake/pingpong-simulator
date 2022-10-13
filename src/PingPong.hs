module PingPong where

import           PingPong.Model

-- import           PingPong.Simulation.Recording
import           PingPong.Simulation.Realtime
import           PingPong.Player

import           PingPong.Submission
import qualified PingPong.Submission.ExampleNativeSubmission as ExampleNativeSubmission

import           PingPong.Communication.Interface
import           PingPong.Communication.JSON
import           Data.Aeson
import PingPong.Grading

-- If your player is implemented in Haskell use the following:


-- main :: IO ()
-- main = do
--   let submission = nativeToIOSubmission ExampleNativeSubmission.submission

--   p1 <- makePlayer submission
--   p2 <- makePlayer $ nativeToIOSubmission $ ExampleNativeSubmission.submission

--   playWithSubmission submission p1 p2
-- --  danceContest submission p1 p2
-- --  gradeSubmission "B2" submission

--   return ()

-- If your player is implemented in Python use the following:
main :: IO ()
main = do
  let submission = readInterfaceSubmission "BatmanB4"

  p1 <- makePlayer submission
  p2 <- makePlayer $ nativeToIOSubmission $ ExampleNativeSubmission.submission

  -- playWithSubmission submission p1 p2
  --  danceContest submission p1 p2
  gradeSubmission "B4" submission

  return ()
