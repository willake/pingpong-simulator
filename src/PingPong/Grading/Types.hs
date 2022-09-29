module PingPong.Grading.Types where

import PingPong.Model hiding (score)

import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import Data.Aeson
import Control.Monad
import Control.Lens

import GHC.Generics

type Assignment = String -- B1 to B7.

reportsDir :: FilePath
reportsDir = "output/reports/"

testcaseDir :: FilePath
testcaseDir = "input/testcases/"

-- rename these:
fileDir :: Assignment -> FilePath
fileDir a = testcaseDir ++ a ++ "/"

filePath :: Assignment -> Int -> FilePath
filePath a i = fileDir a ++ show i ++ ".testcase"


data TestCaseRef = TestCaseRef
  { asid  :: Assignment
  , tcid  :: Int
  , value :: Int
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   TestCaseRef     where toEncoding = genericToEncoding defaultOptions
instance FromJSON TestCaseRef

data TestResult = TestResult
  { trid    :: Int
  , score   :: Int
  , success :: Bool
  , message :: String
  } deriving (Show, Eq, Ord, Generic)

failure :: TestResult
failure = TestResult
  { trid    = 0
  , score   = 0
  , success = False
  , message = ""
  }

treshold :: Float
treshold = 0.001

class AlmostEq a where
  (~=) :: a -> a -> Bool

infix 4 ~=

instance AlmostEq Float where
  x ~= y = abs (x - y) < treshold 

instance AlmostEq Pnt where
  Point2 px py ~= Point2 qx qy = px ~= qx && py ~= qy

instance AlmostEq Vec where
  Vector2 px py ~= Vector2 qx qy = px ~= qx && py ~= qy

instance AlmostEq Seg where
  s1 ~= s2 = s1 ^. start . core ~= s2 ^. start . core && s1 ^. end . core ~= s2 ^. end . core

instance AlmostEq Joint where
  j1 ~= j2 = jang j1 ~= jang j2 && jvel j1 ~= jvel j2

instance AlmostEq a => AlmostEq (Maybe a) where
  Just x  ~= Just y  = x ~= y
  Nothing ~= Nothing = True
  _       ~= _       = False

instance AlmostEq a => AlmostEq [a] where
  xs ~= ys = length xs == length ys && and (zipWith (~=) xs ys)