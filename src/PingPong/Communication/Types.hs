{-# LANGUAGE DeriveAnyClass #-}

module PingPong.Communication.Types where

import Control.Exception

type Port = Int

data InterfaceException = InterfaceException String deriving (Show, Exception)
