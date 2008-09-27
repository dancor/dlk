import Control.Arrow
import Data.Maybe
import FUtil

f s = let
  (w, d) = second (dropWhile (== ' ')) . fromJust $ breakOnSubl "  " s
  in [w, "  " ++ d, ""]

main = interactL (concatMap f)
