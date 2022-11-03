module PingPong.Grading.Types where

import PingPong.Model hiding (score)

import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import Data.Aeson
import Control.Monad
import Control.Lens

import GHC.Generics

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific

type Assignment = String -- B1 to B7.

reportsDir :: FilePath
reportsDir = "output/reports/"

recordingsDir :: FilePath
recordingsDir = "output/recordings/"

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

type Pic = Drawing PixelRGBA8 ()

data TestResult = TestResult
  { trid      :: Int
  , score     :: Int
  , success   :: Bool
  , message   :: String
  , recording :: [Pic]
  } deriving (Generic)

instance Show TestResult where show res1 = "TestResult" ++ show (trid res1, score res1, success res1)
instance Eq   TestResult where res1 == res2 = (trid res1, score res1, success res1) == (trid res2, score res2, success res2)
instance Ord  TestResult where compare res1 res2 = compare (trid res1, score res1, success res1) (trid res2, score res2, success res2)

testResult :: TestResult
testResult = TestResult
  { trid      = 0
  , score     = 0
  , success   = True
  , message   = ""
  , recording = []
  }

failure :: TestResult
failure = TestResult
  { trid      = 0
  , score     = 0
  , success   = False
  , message   = ""
  , recording = []
  }
