module Strace.Parser
    (
     -- Trace file parsing
     parseStrace,
     Line(..),
     TraceEvent(..),
     TimeStamp(..),

     -- Action Parsing (e.g. open() exec())
     Action(..),
     parseAction,
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
import qualified Data.Map as Map

data Line = Line { pid :: ! Int, time :: ! TimeStamp, rest :: TraceEvent }
            deriving Show

data TraceEvent = TraceEvent B.ByteString
                | Unfinished B.ByteString
                | Resumed String B.ByteString
                  deriving Show

type TimeStamp = UTCTime

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


data Action = Signal { name :: String }
            | Unknown { name :: String, args :: B.ByteString }
            | SCexit_group { name :: String, retVal :: Int }
              deriving Show

parseAction :: B.ByteString -> Action
parseAction bs = case parse parseAction' "" bs of
                   Left err -> Unknown "parseerror" (B.pack $ show err)
                   Right a -> a

parseAction' :: Parser Action
parseAction' = parseSignal <|> parseSyscall

parseSignal :: Parser Action
parseSignal = do
  spaces
  string "---"
  spaces
  n <- parseIdent
  spaces
  many anyChar
  return $ Signal n

parseSyscall :: Parser Action
parseSyscall = do
  spaces
  n <- parseIdent
  let p = findSyscallParser n
  p n

findSyscallParser :: String -> (String -> Parser Action)
findSyscallParser id = Map.findWithDefault parseUnknownSyscall id syscallParserMap

syscallParserMap :: Map.Map String (String -> Parser Action)
syscallParserMap = Map.fromList [
                    ("exit_group", parseSC'exit_group)
                   ]

parseUnknownSyscall :: String -> Parser Action
parseUnknownSyscall n = do
  rest <- getInput
  return $ Unknown n rest

parseSC'exit_group :: String -> Parser Action
parseSC'exit_group n = do
  code <- parens parseInt
  spaces
  char '='
  spaces
  char '?'
  return $ SCexit_group n code

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')
