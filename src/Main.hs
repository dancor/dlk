module Main where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.List
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO

data Entry = Entry {
  eWord :: String,
  eDef :: String,
  eIpa :: String,
  ePart :: String
  } deriving Show

data Options = Options {
  oWord :: Maybe String,
  oDef :: Maybe String,
  oFWord :: Maybe String,
  oFDef :: Maybe String,
  oExactWord :: Maybe String,
  oExactDef :: Maybe String,
  oAll :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
  oWord = Nothing,
  oDef = Nothing,
  oFWord = Nothing,
  oFDef = Nothing,
  oExactWord = Nothing,
  oExactDef = Nothing,
  oAll = False}

options :: [OptDescr (Options -> Options)]
options = [
  Option ['w'] ["word"]
    (ReqArg (\ w o -> o {oFWord = Just w}) "WORD") "",
  Option ['d'] ["def-word"]
    (ReqArg (\ w o -> o {oFDef = Just w}) "WORD") "",
  Option ['W'] ["word-part"] (ReqArg (\ w o -> o {oWord = Just w}) "PART") "",
  Option ['D'] ["def-part"] (ReqArg (\ w o -> o {oDef = Just w}) "PART") "",
  Option ['e'] ["word-exact"]
    (ReqArg (\ w o -> o {oExactWord = Just w}) "WORD") "",
  Option ['E'] ["def-exact"]
    (ReqArg (\ w o -> o {oExactDef = Just w}) "DEF") "",
  Option ['a'] ["all results"] (NoArg (\ o -> o {oAll = True})) ""]

procDict :: [String] -> [Entry]
procDict [] = []
procDict ("":rest) = procDict rest
procDict (w:rest) = if "00-" `isPrefixOf` w
  then procDict $ dropWhile ("  " `isPrefixOf`) rest
  else e : procDict rest'
  where
  (wordSp, meta) = break (== '[') w
  word = if null meta || null wordSp then wordSp else init wordSp
  (ipa, partPre) = break (== ']') meta
  (d, rest') = first (concatMap (drop 2)) $ span ("  " `isPrefixOf`) rest
  e = Entry {
    eWord = word,
    eDef = d,
    eIpa = ipa,
    ePart = drop 1 partPre}

partOk :: Maybe String -> Maybe String -> Maybe String -> String -> Bool
partOk subXMb fSubXMb fMatchXMb x =
  onLowJustOrTrue (flip isInfixOf) (lowerMb subXMb) &&
  onLowJustOrTrue
    (\ l w -> w `elem` (map (filter (\ c -> isAlpha c || c == '-')) $ words l))
    (lowerMb fSubXMb) &&
  onLowJustOrTrue (==) (lowerMb fMatchXMb)
  where
  onLowJustOrTrue :: (String -> a -> Bool) -> (Maybe a) -> Bool
  onLowJustOrTrue f = maybe True (f $ map toLower x)
  lowerMb = fmap (map toLower)

lineOk :: Options -> Entry -> Bool
lineOk o e =
  partOk (oWord o) (oFWord o) (oExactWord o) (eWord e) &&
  partOk (oDef o) (oFDef o) (oExactDef o) (eDef e)

filterRes :: Options -> [Entry] -> [Entry]
filterRes opts = (if oAll opts then id else take 20) . filter (lineOk opts)

showDef :: Entry -> String
showDef (Entry word def _ipa part) =
  word ++ " " ++ part ++ ":\n  " ++ def ++ "\n"

langAbbrs :: [(Char, String)]
langAbbrs = [
  ('d', "deu"),
  ('e', "eng"),
  ('f', "fra"),
  ('h', "hin"),
  ('j', "jpn"),
  ('l', "lat"),
  ('o', "epo"),
  ('p', "por"),
  ('r', "rus"),
  ('s', "spa")]

main :: IO ()
main = do
  args <- getArgs
  let
    usageHeader = "usage: dlk <lang> [options] [word]"
    langFooter = intercalate "\n" $
      ["lang options:"] ++
      map (\ (l, lang) -> "  " ++ [l] ++ " -> " ++ lang) langAbbrs
    doErr = error . (++ langFooter) . (++ usageInfo usageHeader options)
    (moreArgs, optsPre) = case getOpt Permute options args of
      (o, n, []) -> (n, foldl (flip id) defaultOptions o)
      (_, _, errs) -> doErr $ concat errs
    (lang, opts) = case moreArgs of
      [lang] -> (lang, optsPre)
      [lang, wd] -> (lang, optsPre {oFWord = Just wd})
      _ -> doErr ""
    stdLoc s = "/usr/share/dictd/freedict-" ++ s ++ ".dict"
    langExp l = maybe (error "unknown lang") id $ lookup l langAbbrs
    langF l = case l of
      l1:l2:[] -> stdLoc $ langExp l1 ++ "-" ++ langExp l2
      l1:[] -> langF (l1:"e")
      _ -> error "unknown lang"
  c <- readFile $ langF lang
  putStr . unlines . map showDef . filterRes opts . procDict $ lines c
