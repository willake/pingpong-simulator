module PingPong.Grading.B6.Types  where

import PingPong.Model
import PingPong.Grading.Types

import Data.Geometry hiding (init, head, replicate)

-- ASSIGNMENT NUMBER

assignment :: Assignment
assignment = "B6"

-- TEST CASES

type TestInput  = ()
type TestOutput = ()
type TestCase   = (TestCaseRef, TestInput, TestOutput)
