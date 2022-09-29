{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module PingPong.Communication.Socket where


import PingPong.Model hiding (prepare, terminate)
import PingPong.Player
import PingPong.Submission
import PingPong.Simulation.Collision
import PingPong.Communication.JSON

import Network.Simple.TCP
import Network.Socket.ByteString as ByteString
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as BSL
import Data.Geometry
import Data.Ext
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Default

import System.Process
import Control.Concurrent
import Control.Exception

import Foreign.C
import Foreign.C.String

import Data.Colour
import Data.Colour.Names


foreign import ccall "PingPong/C/player.h callpython"
  callpython :: CString -> IO CString

type Port = Int

data SocketException = SocketException String deriving (Show, Exception)

-- | Given a unique port and the filepath to a python source file,
--   create a submission. This does not start a python process yet.
readSocketSubmission :: Port -> FilePath -> IOSubmission
readSocketSubmission port path = IOSubmission
  { isNative          = False
  , prepare           = socketPrepare port path
  , terminate         = socketTerminate port
  , detectCollisionIO = socketDetectCollision port
  , handleCollisionIO = socketHandleCollision port
  , controlArmIO      = socketControlArm port
  , evaluateArmIO     = socketEvaluateArm port
  , danceIO           = socketDance port
  , inverseIO         = socketInverse port
  , planIO            = socketPlan port
  , actionIO          = socketAction port
  }

gradient :: Float -> Colour Float
gradient x = blend x forestgreen lawngreen

-- FOR EXERCISE B1 --

socketPrepare :: Port -> FilePath -> IO (String, Arm)
socketPrepare port path = do
  message <- newCString "{\"type\" : \"setup\"}\n"
  canswer <- callpython message
  sanswer <- peekCString canswer
  let answer = BSUTF8.fromString sanswer

  let extracted = assumeNoErrorEither $ eitherDecodeStrict answer
      getName obj = do name <- obj .: "name" :: Parser String
                       return name
      getArm obj = do arm <- obj .: "arm" :: Parser Arm
                      return arm

      thename = assumeNoErrorEither $ parseEither getName extracted
      thearm = assumeNoErrorEither $ parseEither getArm extracted

      result = (thename, thearm)

  return result

socketTerminate :: Port -> IO ()
socketTerminate _ = return ()

ignoreIOExeption :: Default a => IO a -> IO a
ignoreIOExeption = handle ignoreExeption

ignoreExeption :: Default a => IOException -> IO a
ignoreExeption e = do
  putStrLn $ "IOException: " ++ show e
  return def

assumeNoError :: Maybe a -> a
assumeNoError (Just x) = x
assumeNoError Nothing  = error "assumeNoError: But... you told me there would be no error, and there is an error!"

assumeNoErrorEither :: Either String a -> a
assumeNoErrorEither (Right x) = x
assumeNoErrorEither (Left  s) = throw $ SocketException s


-- FOR THE OTHER EXERCISES --

socketMessage port input = do
  let lazymessage = encode input
  let message = BSUTF8.fromString $ show lazymessage
  canswer <- BS.useAsCString message callpython
  sanswer <- peekCString canswer
  let answer = BSUTF8.fromString $ sanswer
  return $ assumeNoErrorEither $ eitherDecode $ read (BSUTF8.toString $ answer)

socketMessageParser port errorHandler parser message = do
  let smessage = BSL.toStrict $ encodingToLazyByteString message
  canswer <- BS.useAsCString smessage callpython
  sanswer <- peekCString canswer
  let answer = BSUTF8.fromString $ sanswer
  let parsed = errorHandler $ parser $ assumeNoErrorEither $ eitherDecodeStrict answer
  return parsed
  
  -- B2

socketDetectCollision :: Port -> Snapshot -> Snapshot -> IO (Maybe Second)
socketDetectCollision port snap1 snap2 = 
  socketMessageParser port id parseCollisionDetection $ pairs ("type" .= ("collision_detection" :: String) <>
                                                         "snapshot1" .= (encodeSnapshot snap1) <>
                                                         "snapshot2" .= (encodeSnapshot snap2))

  -- B3

socketHandleCollision :: Port -> Snapshot -> Snapshot -> Second -> IO (Pnt, Vec)
socketHandleCollision port snap1 snap2 time = 
  socketMessageParser port assumeNoErrorEither parseCollisionHandling $ pairs ("type" .= ("collision_handling" :: String) <>
                                                        "snapshot1" .= (encodeSnapshot snap1) <>
                                                        "snapshot2" .= (encodeSnapshot snap2) <>
                                                        "time" .= time)
  
  -- B4

socketControlArm      :: Port -> Second -> Control -> Arm -> IO Arm
socketControlArm port time control arm = 
  socketMessageParser port assumeNoErrorEither parseControlArm $ pairs ("type" .= ("control_arm" :: String) <>
                                                 "time" .= time <>
                                                 "control" .= control <>
                                                 "arm" .= arm)

socketEvaluateArm     :: Port -> Arm -> IO [Pnt]
socketEvaluateArm port arm = 
  socketMessageParser port assumeNoErrorEither parseEvaluateArm $ pairs ("type" .= ("evaluate_arm" :: String) <>
                                                  "arm" .= arm)

socketDance           :: Port -> Second -> Arm -> IO Control
socketDance port time arm = 
  socketMessageParser port assumeNoErrorEither parseDance $ pairs ("type" .= ("dance" :: String) <>
                                            "time" .= time <>
                                            "arm" .= arm)

  -- B5

socketInverse         :: Port -> Arm -> Seg -> IO (Maybe [Radian])
socketInverse port arm segment = 
  socketMessageParser port assumeNoErrorEither parseInverse $ pairs ("type" .= ("inverse" :: String) <> 
                                              "arm" .= arm <> 
                                              "segment" .= segment)

  -- B6

socketPlan            :: Port -> Second -> Arm -> Second -> Seg -> Vec -> IO Control
socketPlan port current_time current_arm goal_time goal_segment goal_velocity = 
  socketMessageParser port assumeNoErrorEither parsePlan $ pairs ("type" .= ("plan" :: String) <> 
                                            "current_time" .= current_time <>
                                            "current_arm" .= current_arm <>
                                            "goal_time" .= goal_time <>
                                            "goal_segment" .= goal_segment <> 
                                            "goal_velocity" .= goal_velocity)

  -- B7

socketAction          :: Port -> Second -> Item -> Arm -> BallState -> IO Control
socketAction port time item arm state = 
  socketMessageParser port assumeNoErrorEither parseAction $ pairs ("type" .= ("action" :: String) <> 
                                              "time" .= time <> 
                                              "lasthit" .= item <>
                                              "arm" .= arm <> 
                                              "state" .= state)

