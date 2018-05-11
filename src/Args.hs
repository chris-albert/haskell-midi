{-# LANGUAGE DeriveDataTypeable #-}
module Args where

import System.Console.CmdArgs

data MidiArgs = MidiArgs 
  {
    file :: String
  }
  deriving (Data, Typeable, Show)

midiArgs = MidiArgs
  {
    file = def &= args &= typFile
  }

getArgs :: IO MidiArgs
getArgs = cmdArgs midiArgs


