{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

import Data.List as List
import qualified Control.Monad as Monad

--+
import Data.Char
import Data.String as String

str1 :: String
''  = "test1"

str2 :: String
〃  = "test2"

str3 :: String
str3 = str4 where
   str4 :: String
   '' = "test3"

isPos :: Int -> Bool
''  x | x > 0 = True
      |       = False

isPos' :: Int -> Bool
''  = \case
   x | x > 0 -> True
     |       -> False

isPosU ∷ Int → Bool
”   = \case
   x | x > 0 → True
     |       → False

list :: [Int]
''  = [
   1,
   2,
   ]

list' :: [Int]
''  = [
   , 1
   , 2
   ]

data Test =
   | Test1
   | Test2

tabbed :: Int
''  = x where
	x = 1

number :: Int
number = 299_792_458.123_456

