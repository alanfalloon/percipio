module Main where

import Strace
import qualified Data.ByteString.Lazy.Char8 as B 


main = do
  c <- B.getContents
  let (err,lns) = parseStrace c
  mapM_ print err
  mapM_ print lns
