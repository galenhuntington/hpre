{-# LANGUAGE TypeApplications #-}

import Paths_hpre
import System.Directory
import System.Process
import Control.Exception
import System.Exit
import Data.List
import Text.Printf

-- import Control.Monad


runHpre :: String -> IO (String, String)
runHpre inp = do
   hpre <- (++ "/hpre") <$> getBinDir
   (ec, out, err) <- readProcessWithExitCode hpre [] inp
   pure (case ec of ExitSuccess -> out; _ -> "", err)

getOutput :: FilePath -> IO String
getOutput fn =
   catch @IOException (readFile fn) (const $ pure "")

runTest :: FilePath -> IO Bool
runTest file = do
   printf "   %-20s" file
   inp <- readFile file
   (out, err) <- runHpre inp
   let matches tgt ext = (== tgt) <$> getOutput (file ++ ext)
   ok <- (&&) <$> matches out ".out" <*> matches err ".err"
   putStr $ if ok then "\027[01;32mOK" else "\027[01;31mFAIL"
   putStrLn "\027[00m"
   pure ok

main = do
   putStrLn "\nRunning tests...."
   testDir <- (++ "/tests") <$> getDataDir
   setCurrentDirectory testDir
   tests <- filter (".hs" `isSuffixOf`) <$> listDirectory testDir
   res <- and <$> traverse runTest (sort tests)
   exitWith $ if res then ExitSuccess else ExitFailure 1

