-- |
-- Module      :  Numeric.Wrapper.R.GLPK.Phonetics.Ukrainian.Durations
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Can be used to calculate the durations of the approximations of the Ukrainian phonemes
-- using some prepared text with its correct (at least mostly) pronunciation.
-- The prepared text is located in the same directory and contains lines --- the
-- Ukrainian word and its duration in seconds separated with whitespace.
-- The library is intended to use the functionality of the :
--
-- 1) R programming language https://www.r-project.org/
--
-- 2) Rglpk library https://cran.r-project.org/web/packages/Rglpk/index.html
--
-- 3) GNU GLPK library https://www.gnu.org/software/glpk/glpk.html
--
-- For more information, please, see the documentation for them.
--
-- For the model correctness the js here refers to \"ABCEFXYabcdefghijklmnopqrstuvxyz\".
-- To get such js some of the Ukrainian words in the abovementioned file must contain an apostrophe and there should be
-- somewhat like \"їх_друг\".

{-# LANGUAGE CPP #-}

module Numeric.Wrapper.R.GLPK.Phonetics.Ukrainian.Durations where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import Data.Monoid hiding (mconcat)
import Text.Read
import Data.Maybe
import CaseBi (getBFst')
import qualified Data.Vector as VB
import Numeric
import Data.List (intercalate)
import Data.Lists.FLines (newLineEnding)
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif


createCoeffsObj :: Int -> [String] -> [Double]
createCoeffsObj l xss
  | length xss < l = f (xss  `mappend`  replicate (l - length xss) "1.0")
  | otherwise = f (take l xss)
      where f tss = map (\ts -> fromMaybe 1.0 (readMaybe ts::Maybe Double)) tss

countCharInWords :: [String] -> Char -> [Int]
countCharInWords xss x
  | null xss = []
  | otherwise = map (length . filter (== x)) xss

matrix1Column :: [String] -> String -> Char -> [Int]
matrix1Column xss js x = pairwiseComparings x (countCharInWords xss x  `mappend`  rs  `mappend`  rs)
  where l =  length js
        iX = fromMaybe (-l - 1) (VB.findIndex (== x) . VB.fromList $ js)
        rs = if iX < 0 then [] else replicate iX 0  `mappend`  [1]  `mappend`  replicate (l - 1 - iX) 0

pairwiseComparings :: Char -> [Int] -> [Int]
pairwiseComparings x ys = ys `mappend` pairs' x

pairs' :: Char -> [Int]
pairs' x
  | x == 'f' =  [10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  | x == 'v' =  [-5,-20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  | x == 'x' =  [0,0,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  | x == 'h' =  [0,0,-7,-13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  | x == 'g' =  [0,0,0,0,10,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  | x == 'k' =  [0,0,0,0,-7,-13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  | x == 'j' =  [0,0,0,0,0,0,10,10,0,0,10,10,0,0,0,0,0,0,0,0,10,10,0,0]
  | x == 'B' =  [0,0,0,0,0,0,-10,-15,0,0,0,0,0,0,0,0,0,0,10,10,0,0,0,0]
  | x == 'A' =  [0,0,0,0,0,0,0,0,10,10,0,0,0,0,0,0,0,0,0,0,0,0,10,10]
  | x == 'z' =  [0,0,0,0,0,0,0,0,-10,-15,0,0,0,0,0,0,10,10,0,0,0,0,0,0]
  | x == 'd' =  [0,0,0,0,0,0,0,0,0,0,-7,-13,0,0,10,10,0,0,0,0,0,0,0,0]
  | x == 'b' =  [0,0,0,0,0,0,0,0,0,0,0,0,10,10,0,0,0,0,0,0,0,0,0,0]
  | x == 'p' =  [0,0,0,0,0,0,0,0,0,0,0,0,-7,-13,0,0,0,0,0,0,0,0,0,0]
  | x == 't' =  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,-7,-13,0,0,0,0,0,0,0,0]
  | x == 's' =  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-7,-13,0,0,0,0,0,0]
  | x == 'F' =  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-7,-13,0,0,0,0]
  | x == 'E' =  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-8,-14,0,0]
  | x == 'c' =  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-8,-14]
  | otherwise =  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

-- | Actually @n@ is a 'length' bss.
matrixLine :: [String] -> String -> Int -> String
matrixLine bss js n
  | null bss || n <=0 = []
  | otherwise = "mat1 <- matrix(c("  `mappend`  (intercalate ", " . map show $ (concatMap (matrix1Column (bss  `mappend`  bss) js) js))  `mappend`  "), nrow = "  `mappend`  show (2 * n + 2 * length js + 24)  `mappend`  ")" `mappend` newLineEnding

objLine :: VB.Vector Double -> String
objLine v
 | VB.length v >= 32 = "obj1 <- c("  `mappend`  (intercalate ", " . map (\t -> showFFloat Nothing t "") $ objCoeffs v)  `mappend`  ")" `mappend` newLineEnding
 | otherwise = error "Numeric.Wrapper.R.GLPK.Phonetics.Ukrainian.Durations.objLine: Not defined for the short argument. "

objCoeffs :: VB.Vector Double -> [Double]
objCoeffs v =
  let tuple = (1.0,VB.fromList [(0, VB.unsafeIndex v 14), (1, VB.unsafeIndex v 16), (2, VB.unsafeIndex v 9), (3, VB.unsafeIndex v 17),
        (4, VB.unsafeIndex v 18), (5, VB.unsafeIndex v 1), (6, VB.unsafeIndex v 2), (7, VB.unsafeIndex v 3), (8, VB.unsafeIndex v 19),
          (9, VB.unsafeIndex v 20), (10, VB.unsafeIndex v 21), (11, VB.unsafeIndex v 4), (12, VB.unsafeIndex v 22), (13, VB.unsafeIndex v 23),
            (14, VB.unsafeIndex v 24), (15, VB.unsafeIndex v 5), (16, VB.unsafeIndex v 25), (17, VB.unsafeIndex v 26), (18, VB.unsafeIndex v 10),
              (19, VB.unsafeIndex v 11), (20, VB.unsafeIndex v 12), (21, VB.unsafeIndex v 6), (22, VB.unsafeIndex v 27), (23, VB.unsafeIndex v 0),
                (24, VB.unsafeIndex v 13), (25, VB.unsafeIndex v 28), (26, VB.unsafeIndex v 29), (27, VB.unsafeIndex v 7), (28, VB.unsafeIndex v 14),
                  (29, VB.unsafeIndex v 30), (30, VB.unsafeIndex v 8), (31, VB.unsafeIndex v 31)]) in
                     VB.toList . VB.take 32 . VB.map (getBFst' tuple) . VB.enumFromTo 0 $ 31

maxLine :: String
maxLine = "max1 <- TRUE\n"

dirLine :: [String] -> String -> String
dirLine bss js = "dir1 <- c(\"<"  `mappend`  g "<" bss  `mappend`  "\", \">"  `mappend`  g ">" (bss  `mappend`  map (:[]) js)  `mappend`  "\""  `mappend`  h0 32  `mappend`  h 12  `mappend`  ")" `mappend` newLineEnding
  where g xs ys = (intercalate ("\", \""  `mappend`  xs) . replicate (length ys) $ "=")
        h n = concat . replicate n $ ", \">=\", \"<=\""
        h0 n = concat . replicate n $ ", \"<=\""

rhsLineG :: [Double] -> [Double] -> [Double] -> String
rhsLineG zs xs ys = "rhs1 <- c("  `mappend`  f (xs  `mappend`  ys  `mappend`  zs)  `mappend`  ")" `mappend` newLineEnding
  where f ts = (intercalate ", " . map (\t -> showFFloat Nothing t "") $ ts)

rhsLine :: [Double] -> [Double] -> String
rhsLine = rhsLineG (minDurations  `mappend`  maxDurations  `mappend`  constraintsR1 12)

constraintsR1 :: Int -> [Double]
constraintsR1 n = replicate (2 * n) 0.0

minDurations ::[Double]
minDurations = VB.toList v
  where v = VB.generate 32 (\i -> h i)
        h i
          | i == 23 = 0.02
          | otherwise = getBFst' (0.06,VB.fromList . zip [7,11,15,21,27,30] $ replicate 6 0.2) i

maxDurations :: [Double]
maxDurations = replicate 32 0.3

-- | A variant of the more general 'answer2' where the randomization parameters are used to produce every time being run a new result (e. g. this
-- allows to model accents).
answer :: VB.Vector Double -> [String] -> [Double] -> [Double] -> String -> String
answer = answer2 (-0.003) 0.003 (-0.0012) 0.0012

answer2 :: Double -> Double -> Double -> Double -> VB.Vector Double -> [String] -> [Double] -> [Double] -> String -> String
answer2 min1 max1 min2 max2 lsts bss xs ys js = mconcat ["library(\"Rglpk\")",newLineEnding,objLine lsts,matrixLine bss js (length bss),dirLine bss js,
 rhsLine xs ys,maxLine,newLineEnding,"k <- Rglpk_solve_LP(obj = obj1, mat = mat1, dir = dir1, rhs = rhs1, max = max1)",newLineEnding,
  "y <- runif(32, min = ",showFFloat Nothing (-(abs min1)) ", max = ", showFFloat Nothing (abs max1) ")", newLineEnding,
   "if (k$status == 0){k$solution / mean(k$solution)} else {c()}", newLineEnding,
    "if (k$status == 0){z<- k$solution * 0.02 / k$solution[24] + y; z[24] <- 0.02 + runif(1, min = ", showFFloat Nothing (- (abs min2)) ", max = ",
     showFFloat Nothing (abs max2) "); z;} else {c()}",newLineEnding,
      "if (k$status == 0){sprintf(\"uzpp2DurationN = X.getBFst\' (%.8f, VB.fromList [(UZ \'A\' D, %.8f), (UZ \'A\' K, %.8f), (UZ \'B\' D, %.8f), ",
       "(UZ \'B\' K, %.8f), (UZ \'C\' S, %.8f), (UZ \'D\' N, %.8f), (UZ \'E\' L, %.8f), (UZ \'E\' M, %.8f), (UZ \'F\' L, %.8f), (UZ \'F\' M, %.8f), ",
        "(UZ \'a\' W, %.8f), ",
          "(UZ \'b\' D, %.8f), (UZ \'b\' K, %.8f), (UZ \'c\' D, %.8f), (UZ \'d\' D, %.8f), (UZ \'d\' K, %.8f), (UZ \'e\' W, %.8f), (UZ \'f\' L, %.8f), ",
            "(UZ \'f\' M, %.8f), (UZ \'g\' D, %.8f), (UZ \'g\' K, %.8f), (UZ \'h\' D, %.8f), (UZ \'h\' K, %.8f), (UZ \'i\' W, %.8f), (UZ \'j\' D, %.8f),",
              " (UZ \'j\' K, %.8f), (UZ \'k\' L, %.8f), (UZ \'k\' M, %.8f), (UZ \'l\' S, %.8f), (UZ \'l\' O, %.8f), (UZ \'m\' S, %.8f), ",
                "(UZ \'m\' O, %.8f), (UZ \'n\' S, %.8f), (UZ \'n\' O, %.8f), (UZ \'o\' W, %.8f), (UZ \'p\' L, %.8f), (UZ \'p\' M, %.8f), ",
                  "(UZ \'q\' E, %.8f), (UZ \'r\' S, %.8f), (UZ \'r\' O, %.8f), (UZ \'s\' L, %.8f), (UZ \'t\' L, %.8f), (UZ \'t\' M, %.8f), ",
                    "(UZ \'u\' W, %.8f), (UZ \'v\' S, %.8f), (UZ \'v\' O, %.8f), (UZ \'w\' N, %.8f), (UZ \'x\' L, %.8f), (UZ \'x\' M, %.8f), ",
                      "(UZ \'y\' W, %.8f), (UZ \'z\' D, %.8f), (UZ \'z\' K, %.8f)])\",(z[6] + z[7]) / 2,z[1],z[1],z[2],z[2],z[3],z[26]+z[24],",
                        "z[4],z[4],z[5],z[5],z[8],z[9],z[9],z[10],z[11],z[11],z[12],z[13],z[13],z[14],z[14],z[15],z[15],z[16],z[17],z[17],z[18],",
                          "z[18],z[19],z[19],z[20],z[20],z[21],z[21],z[22],z[23],z[23],z[24],z[25],z[25],z[26],z[27],z[27],z[28],z[29],z[29],",
                            "z[10]+z[24],z[30],z[30],z[31],z[32],z[32])} else {print(\"", newLineEnding, "\")}"]

charReplace :: [Char] -> [Char]
charReplace = concatMap g
 where g x
        | x == '-' = "X"
        | x == '0' = "Y"
        | x == 'w' = "cq"
        | x == 'D' = "sq"
        | otherwise = [x]
