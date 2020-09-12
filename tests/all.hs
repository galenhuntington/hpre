{-# LANGUAGE LambdaCase, UnicodeSyntax, TupleSections #-}

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

nested :: Int -> Bool
''  0 = a where
   a :: Bool
   '' = True
''  _ = b where
   b :: Bool
   b = False

list :: [Int]
''  = [  -- comment
   1,    -- comment
   2,
   ]

list' :: [Int]
''  = [  -- comment
   , 1   -- comment
   , 2   -- comment
   ]

data TestA1 = TestA1 { -- comment
   , field11 :: Bool   -- comment
   , field12 :: ()     -- comment
   }

data TestA2 = TestA2 { -- comment
   field21 :: Bool,    -- comment
   field22 :: (),      -- comment
   }

data TestB = -- comment
   | Test1   -- comment
   | Test2   -- comment

constraint :: (
   , Show a
   , Read a
   ) => a -> a
constraint = read . show

slice :: Int -> Int
''  = fst . (5,)

tabbed :: Int
''  = x where
	x = 1

number :: Double
number = 299_792_458.123_456

