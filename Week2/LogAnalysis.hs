{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Trifecta
import Control.Applicative

parseMessageType :: Parser MessageType
parseMessageType = char 

parseMessage :: String -> LogMessage 
