module Main where

-- to gen .dict files:
-- ~ sudo sh -c 'for f in *.dz; do cat $f | gunzip > ${f/.dz/}; done'

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
  ePart :: String} deriving Show

data Options = Options {
  oWord :: Maybe String,
  oDef :: Maybe String,
  oFWord :: Maybe String,
  oFDef :: Maybe String,
  oAll :: Bool} deriving Show

defaultOptions :: Options
defaultOptions = Options {
  oWord = Nothing,
  oDef = Nothing,
  oFWord = Nothing,
  oFDef = Nothing,
  oAll = False}

options :: [OptDescr (Options -> Options)]
options = [
  Option ['w'] ["full-word"]
    (ReqArg (\ w o -> o {oFWord = Just w}) "WORD") "",
  Option ['d'] ["def-full-word"]
    (ReqArg (\ w o -> o {oFDef = Just w}) "WORD") "",
  Option ['W'] ["word-part"] (ReqArg (\ w o -> o {oWord = Just w}) "WORD") "",
  Option ['D'] ["def-part"] (ReqArg (\ w o -> o {oDef = Just w}) "WORD") "",
  Option ['a'] ["all results"] (NoArg (\ o -> o {oAll = True})) ""]

initM :: [a] -> [a]
initM l = if null l then [] else init l

procDict :: [String] -> [Entry]
procDict [] = []
procDict ("":rest) = procDict rest
procDict (w:rest) = if "00-" `isPrefixOf` w
  then procDict $ dropWhile ("  " `isPrefixOf`) rest
  else let
    (wordSp, meta) = break (== '[') w
    (ipa, partPre) = break (== ']') meta
    (d, rest') = first (concatMap (drop 2)) $ span ("  " `isPrefixOf`) rest
    e = Entry {
      eWord = initM wordSp,
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

partOk :: Maybe String -> Maybe String -> String -> Bool
partOk subXMb fSubXMb x = justOrTrue2 isInfixOf (lM subXMb) (map toLower x)
  && justOrTrue2 (\ x y -> elem x $ words y) (lM fSubXMb) (map toLower x)

lineOk :: Options -> Entry -> Bool
lineOk o e = 
  partOk (oWord o) (oFWord o) (eWord e) && partOk (oDef o) (oFDef o) (eDef e)

filterRes :: Options -> [Entry] -> [Entry]
filterRes opts = (if oAll opts then id else take 20) . filter (lineOk opts)

showDef :: Entry -> [Char]
showDef (Entry word def _ipa part) =
  word ++ " " ++ part ++ ":\n  " ++ def ++ "\n"

main :: IO ()
main = do
  let usageHeader = "usage: deu [options] [lang]"
  args <- getArgs
  let 
    (lang, opts) = case getOpt Permute options args of
      (o, [lang], []) -> (lang, foldl (flip id) defaultOptions o)
      (_, _, errs) -> error (concat errs ++ usageInfo usageHeader options)
    stdLoc s = "/usr/share/dictd/freedict-" ++ s ++ ".dict"
    langExp l = case l of
      'e' -> "eng"
      'f' -> "fra"
      'g' -> "deu"
      'h' -> "hin"
      'j' -> "jpn"
      'l' -> "lat"
      'r' -> "rus"
      'p' -> "por"
      's' -> "spa"
      _ -> error "unknown lang"
    langF l = case l of
      l1:l2:[] -> stdLoc $ langExp l1 ++ "-" ++ langExp l2
      l1:[] -> langF (l1:"e")
      _ -> error "unknown lang"
  c <- readFile $ langF lang
  putStr . unlines . map showDef . filterRes opts . procDict $ lines c
