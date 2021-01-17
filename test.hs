{-# LANGUAGE CPP, TypeApplications #-}

import Data.Functor
import System.Process
import System.Exit
import Control.Exception
import Control.Monad
import System.FilePath

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced (goldenTest)

#ifdef BUILDING
import Paths_hpre
#else
getBinDir = pure ""
getDataDir = pure ""
#endif

runHpre :: String -> IO (String, String)
runHpre inp = do
   hpre <- (++ "/hpre") <$> getBinDir
   (ec, out, err) <- readProcessWithExitCode hpre [] inp
   pure (case ec of ExitSuccess -> out; _ -> "", err)

loadMayFile :: FilePath -> IO String
loadMayFile fn =
   catch @IOException (readFile fn) (const $ pure "")

runTest :: FilePath -> TestTree
runTest ref
   = goldenTest
      name
      ((,) <$> get "out" <*> get "err")
      (runHpre =<< readFile ref)
      (\x y -> pure $ guard (x/=y) $> "")
      (\ (o, e) -> write "out" o *> write "err" e) -- not tried yet
   where
   name = takeFileName ref
   get ext = loadMayFile $ ref <.> ext
   write ext = writeFile $ ref <.> ext

allGoldens :: IO TestTree
allGoldens = do
   putStrLn "\nRunning tests...."
   testDir <- (++ "/tests") <$> getDataDir
   tests <- findByExtension [".hs"] testDir
   pure $ testGroup "golden out/err tests" $ map runTest tests

main :: IO ()
main = defaultMain =<< allGoldens

