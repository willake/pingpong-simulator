module PingPong.Grading.Reports where

import PingPong.Model hiding (score, prepare, terminate)
import PingPong.Grading.Types
import PingPong.Submission
import PingPong.Submissions (submissions)
import PingPong.Communication.Interface

import Data.List
import Data.Char
import Control.Monad
import Control.Lens
import Control.Exception

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific

import System.Directory
import System.Process

frameRate = 50

-- INDIVIDUAL REPORT

writeReport :: String -> Assignment -> [(TestCaseRef, TestResult)] -> IO ()
writeReport name i results = do
  let cs = unlines $ zipWith (rep $ length results) [1..] results
      scores = sum $ map (score . snd) results
      values = sum $ map (value . fst) results
      frames = concatMap (recording . snd) results
      report = "report for " ++ name ++ ", assignment " ++ i ++ "\n"
            ++ replicate 80 '-' ++ "\n" 
            ++ cs
            ++ replicate 80 '-' ++ "\n" 
            ++ show scores ++ " points out of " ++ show values ++ " possible"
            ++ "\n" ++ "grade: " ++ show (round2 $ grade values scores)
  createDirectoryIfMissing True reportsDir
  writeFile (reportsDir ++ systemName name ++ "_" ++ i ++ "_report.txt") report 
  when (not $ null frames) $ exportRecording frames name i

systemName :: String -> String
systemName = filter isAlphaNum

rep :: Int -> Int -> (TestCaseRef, TestResult) -> String
rep n c r = "[case " ++ outOf n c ++ "] " ++ uncurry showResult r

showResult :: TestCaseRef -> TestResult -> String
showResult c r | success r = "success: " ++ outOf (value c) (score r) ++ " points | " ++ message r
               | otherwise = "failure: " ++ outOf (value c) (score r) ++ " points | " ++ message r


outOf :: Show a => a -> a -> String
outOf n c = 
  let sn = show n
      sc = show c
      pc = replicate (length sn - length sc) ' '
  in pc ++ sc ++ "/" ++ sn

grade :: Int -> Int -> Float  
grade values scores = 10 * f scores / f values

f :: Int -> Float
f = fromInteger . toInteger

round2 :: Float -> Float
round2 x = 0.01 * (f $ round $ 100 * x)

-- something for ignoring the worst 10 shots, move?
best :: Int -> [Int] -> Int
best n xs = sum $ take n $ reverse $ sort xs



-- RECORDING REPORT



exportRecording :: [Drawing PixelRGBA8 ()] -> String -> Assignment -> IO ()
exportRecording pics name i = do
  createDirectoryIfMissing True $ recordingsDir ++ "frame/"
  putStr "recording"
  sequence_ $ zipWith exportFrame [1..] pics
  putStrLn "done!"
  let path = recordingsDir ++ systemName name ++ "_" ++ i ++ "_report.mp4"
  createDirectoryIfMissing True recordingsDir
  callCommand $ "ffmpeg -y -r " ++ show frameRate ++ " -f image2 -s 1920x1080 -i " ++ recordingsDir ++ "frame/%d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p " ++ path
  removeDirectoryRecursive $ recordingsDir ++ "frame/"

exportFrame :: Int -> Drawing PixelRGBA8 () -> IO ()
exportFrame i pic = do
  let white = PixelRGBA8 255 255 255 255
      img = renderDrawing 1920 1080 white pic
  writePng (recordingsDir ++ "frame/" ++ show i ++ ".png") img
  putStr $ frameMark i

frameMark :: Int -> String
frameMark i | i `mod` (60 * round frameRate) == 0 = show $ i `div` round frameRate
            | i `mod` (10 * round frameRate) == 0 = "#" 
            | i `mod` round frameRate == 0 = "|" 
            | otherwise                    = "."




-- SUMMARY REPORT

-- | Make a summary report out of all test results
--   TODO: give results as [(IOSubmission, TestCaseRef, TestResult)]
--         and group them here, for more flexibility and robustness?
makeSummary :: Assignment -> [(IOSubmission, [(TestCaseRef, TestResult)])] -> IO ()
makeSummary i srs = do
  let n = length srs
      m = length $ snd $ head $ srs
  subsums :: [(String, Float)]
          <- mapM (submissionSummary n) $ zip [1..] srs
  let (sublines, grades) = unzip subsums
  caslines <- mapM (testCaseSummary' m) $ transpose $ map snd srs
  let report = "summary report for assignment " ++ i ++ "\n"
            ++ replicate 80 '-' ++ "\n" 
            ++ "number of submissions: " ++ show n ++ "\n"
            ++ "number of test cases: " ++ show m ++ "\n"
            ++ replicate 80 '-' ++ "\n" 
            ++ "results per submission" ++ "\n"
            ++ unlines sublines
            ++ "average grade " ++ show (sum grades / f n) ++ "\n"
            ++ replicate 80 '-' ++ "\n" 
            ++ "results per test case" ++ "\n"
            ++ unlines caslines

  writeFile (reportsDir ++ "" ++ i ++ "_summary_report.txt") report

-- | Produce a 1-line summary of results from a single submission.
submissionSummary :: Int -> (Int, (IOSubmission, [(TestCaseRef, TestResult)])) -> IO (String, Float)  
submissionSummary n (i, (sub, results)) = do
  let scores = sum $ map (score . snd) results
      values = sum $ map (value . fst) results
  return (    "[submission " ++ outOf n i ++ "]"
           ++ " score: " ++ outOf values scores 
           ++ " grade: " ++ show (grade values scores)
         , grade values scores )
  -- show name too? but how to get it?

-- | Produce a 1-line summary of results for a single test case.
testCaseSummary :: Int -> TestCaseRef -> [TestResult] -> IO String
testCaseSummary m ref results = do
  let scores = map score results
      mean   = (sum $ map f scores) / (f $ length scores)
      val    = f $ value ref
  return $ "[case " ++ outOf m (tcid ref) ++ "]" ++ " average score: " ++ outOf val mean

testCaseSummary' :: Int -> [(TestCaseRef, TestResult)] -> IO String
testCaseSummary' m tts = testCaseSummary m (fst $ head tts) (map snd tts)
  -- check if all testCaseRefs are the same?