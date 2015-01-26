module Music(module Haskore, playWin, writeMidi, (%)) where

import Haskore
import System.Cmd(system)
import System.Directory(getCurrentDirectory)
import Data.Ratio((%))
import System.Info(os)

----------------------------------------------------------
-- commands for creating and playing music 

-- write music "m" to "file.mid"
-- this file "test.mid" can be double clicked to play
writeMidi m file = outputMidiFile (file++".mid") (testMidi m)     

-- Here attempt to script the playing of music
-- Source location of the windows media player
player = 
  case os of
    "mingw32" -> "\"C:\\Program Files (x86)\\Windows Media Player\\wmplayer.exe\""
    "linux" -> "timidity"
    "mac"   -> "afplay"

-- the appropriate command line flag
command = 
  case os of
    "mingw32" -> " /play /close "
    "linux" -> " "
    "mac"   -> " "
    other -> " "

-- On windows, creates a midi file and plays it on the windows media player
-- On Mac or linux the player and its location will be different
playWin m = 
    do  test m
        dir <- getCurrentDirectory                 
        system (player ++ command ++ dir ++"\\test.mid")  -- a command line call
        return ()
        