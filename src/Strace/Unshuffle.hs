{-# LANGUAGE NamedFieldPuns #-}
module Strace.Unshuffle
    (
     unshuffle,
    ) where

import Strace.Parser

data Process = Process { pid :: Int, events :: [Event] }
               deriving Show

data Event = Event { time :: EventTime, action :: Action }
             deriving Show

data EventTime = Instant TimeStamp
               | Duration TimeStamp TimeStamp
                 deriving Show


unshuffle :: [Line] -> [Process]
unshuffle lns = proc : laterprocs
    where
      ~(proc,otherlns) = extractProcess lns laterprocs
      laterprocs = unshuffle otherlns

extractProcess :: [Line] -> [Process] -> (Process,[Line])
extractProcess ~(lns@(Line {Strace.Parser.pid} : _)) procs =
    (Process { Strace.Unshuffle.pid, events }, lns')
    where
      (events, lns') = extractEvents pid lns procs

extractEvents :: Int -> [Line] -> [Process] -> ([Event], [Line])
extractEvents pid lns procs = error ""
