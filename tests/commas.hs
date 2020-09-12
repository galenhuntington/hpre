{-# LANGUAGE TupleSections #-}

list :: [Int]
list = [  -- comment
   1,    -- comment
   2,
   ]

list' :: [Int]
list' = [  -- comment
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
slice = fst . (5,)

