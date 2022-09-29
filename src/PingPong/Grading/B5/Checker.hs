module PingPong.Grading.B5.Checker (getTestCaseRefs, checkSubmission) where

import PingPong.Grading.B5.Types

import PingPong.Grading.Types
import PingPong.Submission
import PingPong.Simulation.Collision

import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import Control.Monad

import System.Random

getTestCaseRefs :: IO [TestCaseRef]
getTestCaseRefs = return []

checkSubmission :: IOSubmission -> TestCaseRef -> IO TestResult
checkSubmission sub ref = undefined
