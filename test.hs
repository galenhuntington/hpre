{-# LANGUAGE CPP, TypeApplications #-}

import Data.Functor
import System.Process
import System.Exit
import System.Directory (removeFile)
import Control.Exception
import Control.Monad
import System.FilePath
import Data.List (sort)

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced (goldenTest)

#ifdef BUILDING
import Paths_hpre
#else
getBinDir = pure "."
#endif

data OutErr = OutErr !String !String
   deriving (Eq, Show)

runHpre :: String -> IO OutErr
runHpre inp = do
   hpre <- (++ "/hpre") <$> getBinDir
   (ec, out, err) <- readProcessWithExitCode hpre [] inp
   pure $ OutErr (case ec of ExitSuccess -> out; _ -> "") err

loadMayFile :: FilePath -> IO String
loadMayFile fn =
   catch @IOException (strict <$> readFile fn) (const $ pure "")
      where strict x = length x `seq` x

saveMayFile :: FilePath -> String -> IO ()
saveMayFile fn ""  = void $ try @IOException $ removeFile fn
saveMayFile fn dat = writeFile fn dat

runTest :: FilePath -> TestTree
runTest ref
   = goldenTest
      name
      (OutErr <$> get "out" <*> get "err")
      (runHpre =<< readFile ref)
      (\x y -> pure $ guard (x/=y) $> (show x ++ " vs. " ++ show y))
      (\ (OutErr o e) -> write "out" o *> write "err" e)
   where
   name = takeFileName ref
   get ext = loadMayFile $ ref <.> ext
   write ext = saveMayFile $ ref <.> ext

allGoldens :: IO TestTree
allGoldens = do
   putStrLn "\nRunning tests...."
   tests <- sort <$> findByExtension [".hs"] "tests"
   pure $ testGroup "golden out/err tests" $ map runTest tests

main :: IO ()
main = defaultMain =<< allGoldens

