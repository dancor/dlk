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
  oAll = False
  }

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
  Option ['a'] ["all results"] (NoArg (\ o -> o {oAll = True})) ""
  ]

procDict :: [String] -> [Entry]
procDict [] = []
procDict ("":rest) = procDict rest
procDict (w:rest) = if "00-" `isPrefixOf` w
  then procDict $ dropWhile ("  " `isPrefixOf`) rest
  else let
    (wordSp, meta) = break (== '[') w
    word = if null meta then wordSp else init wordSp
    (ipa, partPre) = break (== ']') meta
    (d, rest') = first (concatMap (drop 2)) $ span ("  " `isPrefixOf`) rest
    e = Entry {
      eWord = word,
      eDef = d,
      eIpa = ipa,
      ePart = drop 1 partPre
      }
    in e:procDict rest'

justOrTrue2 :: (a -> b -> Bool) -> (Maybe a) -> b -> Bool
justOrTrue2 f xMb y = case xMb of
  Nothing -> True
  Just x -> f x y

lM :: Maybe [Char] -> Maybe [Char]
lM x = case x of
  Just xx -> Just (map toLower xx)
  Nothing -> Nothing

partOk :: Maybe [Char] -> Maybe [Char] -> Maybe [Char] -> [Char] -> Bool
partOk subXMb fSubXMb fMatchXMb x =
  justOrTrue2 isInfixOf (lM subXMb) (map toLower x) &&
  justOrTrue2
    (\ w l -> w `elem` (map (filter (\ c -> isAlpha c || c == '-')) $ words l))
    (lM fSubXMb) (map toLower x) &&
  justOrTrue2 (==) (lM fMatchXMb) (map toLower x)

lineOk :: Options -> Entry -> Bool
lineOk o e =
  partOk (oWord o) (oFWord o) (oExactWord o) (eWord e) &&
  partOk (oDef o) (oFDef o) (oExactDef o) (eDef e)

filterRes :: Options -> [Entry] -> [Entry]
filterRes opts = (if oAll opts then id else take 20) . filter (lineOk opts)

showDef :: Entry -> [Char]
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
  ('s', "spa")
  ]

main :: IO ()
main = do
  let
    usageHeader = "usage: dlk <lang> [options] [word]"
    langFooter = intercalate "\n" $
      ["lang options:"] ++
      map (\ (l, lang) -> "  " ++ [l] ++ " -> " ++ lang) langAbbrs

  args <- getArgs
  let
    doErr = error . (++ langFooter) . (++ usageInfo usageHeader options)
    (moreArgs, optsPre) = case getOpt Permute options args of
      (o, n, []) -> (n, foldl (flip id) defaultOptions o)
      (_, _, errs) -> doErr $ concat errs
    (lang, opts) = case moreArgs of
      [lang] -> (lang, optsPre)
      [lang, wd] -> (lang, optsPre {oFWord = Just wd})
      _ -> doErr ""
    stdLoc s = "/usr/share/dictd/freedict-" ++ s ++ ".dict"
    langExp l = case lookup l langAbbrs of
      Just lang -> lang
      Nothing -> error "unknown lang"
    langF l = case l of
      l1:l2:[] -> stdLoc $ langExp l1 ++ "-" ++ langExp l2
      l1:[] -> langF (l1:"e")
      _ -> error "unknown lang"
  c <- readFile $ langF lang
  putStr . unlines . map showDef . filterRes opts . procDict $ lines c
