module Strace
    (
     parseStrace,
     Line(..),
     TraceEvent(..),
    ) where

import Data.Either                 (partitionEithers)
import Data.Int                    (Int64)
import Data.Time.Clock             (UTCTime)
import Data.Time.Format            (readTime)
import System.Locale               (defaultTimeLocale)
import Text.Parsec                 (ParseError)
import Text.Parsec.ByteString.Lazy (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import qualified Data.ByteString.Lazy.Char8 as B 

data Line = Line { pid :: ! Int, time :: ! UTCTime, rest :: TraceEvent }
            deriving Show

data TraceEvent = TraceEvent B.ByteString
                | Unfinished B.ByteString
                | Resumed String B.ByteString
                  deriving Show

parseStrace :: B.ByteString -> ([ParseError],[Line])
parseStrace contents = partitionEithers linesParse
    where
      linesText = B.lines contents
      linesParse = map (parse parseLine "") linesText

parseLine :: Parser Line
parseLine = do
  pid <- parseInt
  spaces
  ts <- parseTimeStamp
  spaces
  te <- parseTraceEvent
  return (Line pid ts te)
  

parseDigits :: Parser String
parseDigits = many1 digit

parseDigitsDotDigits :: Parser String
parseDigitsDotDigits = do
  d1 <- parseDigits
  char '.'
  d2 <- parseDigits
  return (d1 ++ "." ++ d2)

parseIdent :: Parser String
parseIdent = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (first:rest)

parseInt :: Parser Int
parseInt = do
  ds <- parseDigits
  return (read ds)

parseTimeStamp :: Parser UTCTime
parseTimeStamp = do
  ds <- parseDigitsDotDigits
  return (readTime defaultTimeLocale "%s%Q" ds)


parseTraceEvent :: Parser TraceEvent
parseTraceEvent = try parseResumed <|> parseTraceEvent'
  where
    parseResumed, parseTraceEvent' :: Parser TraceEvent
    parseResumed = do
      string "<... "
      id <- parseIdent
      string " resumed>"
      rest <- getInput
      return (Resumed id rest)
    parseTraceEvent' = do
      rest <- getInput
      let (pre,suf) = B.splitAt prefixLen rest
          prefixLen, restLen :: Int64
          prefixLen = restLen - unfinishedLen
          restLen = B.length rest
      return $ if unfinished == suf
               then Unfinished pre
               else TraceEvent rest
    unfinished :: B.ByteString
    unfinished = B.pack "<unfinished ...>"
    unfinishedLen :: Int64
    unfinishedLen = B.length unfinished
