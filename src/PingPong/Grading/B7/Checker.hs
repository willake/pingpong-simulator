module PingPong.Grading.B7.Checker (getTestCaseRefs, checkSubmission) where

import PingPong.Grading.B7.Types

import PingPong.Model hiding (score)

import PingPong.Grading.Types

import PingPong.Submission hiding (name, arm, catchErrorCall, catchInterfaceException, catchExceptions)

import PingPong.Simulation
import PingPong.Simulation.Collision
import PingPong.Communication.Interface
import PingPong.Player hiding (name)
import PingPong.Communication.JSON
import PingPong.Communication.Types
import PingPong.Communication.Interface

import PingPong.Draw
import PingPong.Draw.Rasterific

import qualified PingPong.Submission.ModelNativeSubmission as ModelNativeSubmission


import Data.Geometry hiding (init, head, replicate)
import Data.Ext
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.Aeson

import Control.Monad
import Control.Lens
import Control.Exception

import System.Random
import System.Directory

import Codec.Picture( PixelRGBA8( .. ), writePng )
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
  testcase <- getTestCase ref
  testShot player testcase








-- record the test?
frameRate :: Float
frameRate = 50

frameCount :: Int
frameCount = 1000

frameDuration :: Float
frameDuration = 1 / frameRate


testShot :: Player -> TestCase -> IO TestResult 
testShot ip (ref, ib) = do
  let initialState = defState 
                   { p1    = ip {arm = initArm ip}
                   , p2    = noPlayer
                   , phase = DuringRally
                   , ball  = ib
                   }
  putStrLn $ "test shot for " ++ name ip
  Right font <- loadFontFile "fonts/lato/Lato-Bold.ttf" 
  (hits, pics) <- record frameCount font initialState
  return $ testResult { trid      = tcid ref
                      , score     = fst $ grade $ map snd $ reverse hits
                      , success   = True
                      , message   = snd $ grade $ map snd $ reverse hits
                      , recording = map (stamp font $ tcid ref) pics
                      }

record :: Int -> Font -> State -> IO ([Hit], [Pic])
record 0 _    st = return (hits st, [])
record _ _    st | afterRally $ phase st = return (hits st, [])
record n font os = do
  (ns  , pic ) <- step font os
  (hits, pics) <- record (n - 1) font ns 
  return $ (hits, pic : pics)

step :: Font -> State -> IO (State, Pic)
step font os = do
  let sub = nativeToIOSubmission ModelNativeSubmission.submission
  pic <- drawState (evaluateArmIO sub) font os
  ns <- update sub frameDuration os
  return (ns, pic)

stamp :: Font -> Int -> Pic -> Pic
stamp font i p = do p 
                    drawText font (convertColor cTable) 36 300 300 $ "(test shot " ++ show i ++ ")"

afterRally (AfterRally _) = True
afterRally _ = False


grade :: [Item] -> (Int, String)
grade (Bat Opponent : Table Self : Bat Self : Table Opponent : _) = (2, "hit the ball and hit the table")
grade (Bat Opponent : Table Self : Bat Self : _) = (1, "hit the ball, but not the table")
grade _ = (0, "did not hit the ball")
















catchExceptions :: IO TestResult -> IO TestResult
catchExceptions = handle catchErrorCall
                . handle catchInterfaceException

catchInterfaceException :: InterfaceException -> IO TestResult
catchInterfaceException e = return $ failure { message = show e }

catchErrorCall :: ErrorCall -> IO TestResult
catchErrorCall e = return $ failure { message = show e }
