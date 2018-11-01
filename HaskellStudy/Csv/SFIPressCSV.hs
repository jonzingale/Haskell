module SFIPressCSV where

import qualified Data.List as LS
import qualified Data.Char as C
import qualified Data.Set as S

chapter = "./SFIPress/wordsInChapter.csv"
commons = "./SFIPress/5000_Most_Common_English_Words.csv"
filtered = "./SFIPress/filteredChapter.csv"

fChapter = do
  cpp <- readFile chapter
  cmm <- readFile commons
  let cp = S.fromList $ words cpp
  let cm = S.fromList $ words cmm
  let ncm = (S.toList) $ (S.\\) cp cm
  -- filter ncm with lower, but watch the Logic Gates
  return ncm

-- saveArr :: String -> ULattice -> IO()
saveArr = do
  fs <- fChapter
  writeFile filtered $ unlines fs

