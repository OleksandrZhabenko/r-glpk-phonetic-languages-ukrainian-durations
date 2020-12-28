-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Can be used to calculate the approximations of the Ukrainian phonemes
-- using some prepared text with its correct (at least mostly) pronunciation.
-- The prepared text is located in the same directory and contains lines --- the
-- Ukrainian word and its duration in seconds separated with whitespace.
--
-- The executable is intended to use the functionality of the :
--
-- 1) R programming language https://www.r-project.org/
--
-- 2) Rglpk library https://cran.r-project.org/web/packages/Rglpk/index.html
--
-- 3) GNU GLPK library https://www.gnu.org/software/glpk/glpk.html
--
-- For more information, please, see the documentation for them.
--

module Main where

import Data.Char (isAlpha)
import Numeric.Wrapper.R.GLPK.Phonetics.Ukrainian.Durations
import System.Environment (getArgs)
import qualified Data.Vector as VB
import Text.Read
import Data.List
import Data.Maybe (fromMaybe)
import Melodics.ByteString.Ukrainian


main :: IO ()
main = do
 args <- getArgs
 if any isAlpha (concat . take 1 $ args) then processMentG args
 else do
  let min1 = - abs (fromMaybe (-0.003) (readMaybe (concat . take 1 $ args)::Maybe Double))
      max1 = abs (fromMaybe 0.003 (readMaybe (concat . drop 1. take 2 $ args)::Maybe Double))
      min2 = - abs (fromMaybe (-0.0012) (readMaybe (concat . drop 2 . take 3 $ args)::Maybe Double))
      max2 = abs (fromMaybe 0.0012 (readMaybe (concat . drop 3 . take 4 $ args)::Maybe Double))
      arGs = dropWhile (all (not . isAlpha)) args
  processMentG arGs

processMentG :: [String] -> IO ()
processMentG ts = do
  let file = concat . take 1 $ ts
  contents <- readFile file
  let coeff = fromMaybe (sqrt 2.0) ((readMaybe (concat . drop 1 . take 2 $ ts))::Maybe Double)
      lst0 = createCoeffsObj 32 (drop 2 ts)
      lstCfs = VB.fromList lst0
      xss = map words . lines $ contents
      words2 = map head xss
      lengths0 = map ((\ts -> read ts::Double) . last) xss
      bss = map (sort . charReplace . convertToProperUkrainianS) words2
      js = tail . VB.toList . VB.uniq . VB.fromList . sort . unwords $ bss
  putStrLn . answer lstCfs bss (map (*coeff) lengths0) (map (* (1.0 / coeff)) lengths0) $ js

