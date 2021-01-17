{-# LANGUAGE CPP #-}

import System.Environment
import Data.Char
import Control.Monad.State
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Version
import Text.Read.Lex

#ifdef BUILDING
import Paths_hpre (version)
#else
version = Version [] ["n/a"]
#endif


--  YMMV
tabWidth = 3 :: Int

{-# INLINE __ #-}
__ = True  -- second-best

warn :: String -> a -> a
warn = trace . ("Warning: " ++)

abort :: String -> a
abort = errorWithoutStackTrace

--  Yank up to end of quote.
--  TODO multiline quote support
skipQuote :: String -> (String, String)
skipQuote = go "" where
   go q []            = (q, [])
   go q "\\"          = (q++['\\'], [])
   go q ('"':s)       = (q++"\"", s)
   go q ('\\':'\\':s) = go (q++"\\\\") s
   go q ('\\':'"':s)  = go (q++"\\\"") s
   go q (c:s)         = go (q++[c]) s

untab :: String -> String
untab = go (0::Int) where
   go _ [] = []
   go m ('\t':l) = replicate (tabWidth-(m`mod`tabWidth)) ' ' ++ go 0 l
   go m (c:l) = c : go (m+1) l

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c=='\'' || c=='_'

--  Allow "ticks" or "underscores" as digit separators.
--  That is, not settling on one proposal yet.
--  With NumericUnderscores an accepted extension, this is deprecated.
--  It should be removed eventually, as it can interfere with alignment
--  (and that isn't worth fixing since deprecated).
tickedNums :: String -> String
tickedNums l@(x:m)
   | isDigit x =
      let (p1, p2) = span (\y -> isDigit y || isTick y) l
          p1' = filter (not.isTick) p1
                  ++ reverse (takeWhile isTick $ reverse p1) -- prob not needed
      in (if p1/=p1' && '\'' `elem` p1 then warn "Ticked numbers are deprecated." else id)
            $ p1' ++ tickedNums p2
   | isNameChar x =
      let (nm, rest) = span isNameChar l
      in nm ++ tickedNums rest
   | x=='\\' || x=='\'', mh:mt <- m
               = x : mh : tickedNums mt
   | x=='"'    = let (a, b) = skipQuote m in '"' : a ++ tickedNums b
   | __        = x : tickedNums m
  where
   isTick c = c == '\'' || c == '_'
tickedNums _       = []

--  Empty guards are filled in with True.
--  This was formerly monadic and could perhaps be simplified.
emptyGuard :: String -> String
emptyGuard (q:c:s) | q=='\\' || q=='\'' =
   [q, c] ++ emptyGuard s
emptyGuard ('"':s) =
   let (a, b) = skipQuote s in '"' : a ++ emptyGuard b
emptyGuard (c1:'|':c2:s) | isSpace c1 && isSpace c2 =
   c1 : '|' : pfx ++ emptyGuard p2 where
      (p1, p2) = span isSpace s
      p2' = takeWhile isSymbolChar p2
      pfx 
         | p2' `elem` ["=", "→", "->"] =
            case length p1 of
               x | x<4  -> "True"
                 | __   -> c2 : "True" ++ drop 4 p1
         | __ = c2 : p1
emptyGuard (c:s) = c : emptyGuard s
emptyGuard _ = []

columnPragma :: Int -> String
columnPragma col = "{-#COLUMN " ++ show col ++ "#-}"

type DittoHist = [(Int, String)]

histAtIndent :: Int -> DittoHist -> (Maybe String, DittoHist)
histAtIndent ind hist =
   case dropWhile ((> ind) . fst) hist of
      (ind', s) : hist' | ind == ind' -> (Just s, hist')
      hist'                           -> (Nothing, hist')

--  This recomputes some stuff for better separation.
expandDitto :: String -> String -> String
expandDitto val line =
   pre ++ val ++ columnPragma (length (pre ++ ditto ++ sps) + 1) ++ rest
   where
      (pre, l1)   = span isSpace line
      (ditto, l2) = break isSpace l1
      (sps, rest) = span isSpace l2

letFinderS :: String -> [(Int, String)] -> [(Int, String)]
letFinderS line = flip loop chunks where
   chunks = unfoldr chunk (0, line)
   chunk (_, "") = Nothing
   chunk (col, s) = Just ((s1, s2, col'), (col' + length s2, s3)) where
      (s1, s1') = break isNameChar s
      (s2, s3) = span isNameChar s1'
      col' = col + length s1
   loop ac ((_, "let", _) : (sps, nm, ind) : rest)
      | all isSpace sps = loop ((ind, nm) : ac) rest
   loop ac (_ : rest)   = loop ac rest
   loop ac _            = ac

--  "Ditto marks" for repeated names in definitions.
--  TODO? allow mid-line names to be ditto'ed
dittoM :: String -> State DittoHist String
dittoM line = do
   let (sp1, rest) = span isSpace line
       indent = length sp1
   (cur, hist) <- gets $ histAtIndent indent
   let put' = put . letFinderS line
   let doDitto = case cur of
         Just val -> do
            put' $ (indent, val) : hist
            pure $ expandDitto val line
         _        -> abort $ "Orphaned ditto mark:  " ++ line
   case rest of
      '\'':'\'':x:_ | isSpace x         -> doDitto
      c:x:_ | c `elem` "”〃", isSpace x -> doDitto
      c:_ | isLower c                   -> do
         put' $ (indent, takeWhile isNameChar rest) : hist
         pure line
      []        -> pure line
      '-':'-':_ -> pure line
      '{':'-':_ -> pure line
      '-':'}':_ -> pure line
      _         -> put' hist *> pure line

dittoMarks :: [String] -> [String]
dittoMarks inp = flip evalState [] $ traverse dittoM inp

--  Strips final comments and also spaces while it's at it.
--  TODO can get messed up by quotes
decomment :: String -> String
decomment s
   | null s' || take 2 s' == "--" = []
   | __ =
      if isSymbolChar c
         then let (a, b) = span isSymbolChar s in a ++ decomment b
         else c : decomment rest
   where
   s'     = dropWhile isSpace s
   c:rest = s

stripSpace :: String -> String
stripSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--  If c is the first non-space character, replace with space.
spaceOut :: Char -> String -> Maybe String
spaceOut c s =
   case span isSpace s of
      (sp, c' : s') | c == c' -> Just $ sp ++ ' ' : s'
      _                       -> Nothing

spanCommentLines :: [String] -> ([String], [String])
spanCommentLines = span (null . stripSpace . decomment)

commasR :: [String] -> [String]
commasR []     = []
commasR (l:ls) = (: commasR ls) $ fromMaybe l $ do
   ',' : l' <- pure $ reverse $ decomment l
   nxt : _  <- pure $ snd $ spanCommentLines ls
   c : _    <- pure $ dropWhile isSpace nxt
   guard $ c `elem` "])}"
   pure $ reverse l'

commasL :: [String] -> [String]
commasL []     = []
commasL (l:ls) = fromMaybe (l : commasL ls) $ do
   c : _ <- pure $ reverse $ decomment l
   guard $ c `elem` "{(["
   let (comms, rest) = spanCommentLines ls
   nxt : ls' <- pure rest
   nxt' <- spaceOut ',' nxt
   pure $ l : comms ++ nxt' : commasL ls'

dataBarsL :: [String] -> [String]
dataBarsL []    = []
dataBarsL (l:ls) = fromMaybe (l : dataBarsL ls) $ do
   guard $ "data " `isPrefixOf` l
   '=' : _ <- pure $ reverse $ decomment l
   let (comms, rest) = spanCommentLines ls
   nxt : ls' <- pure rest
   nxt' <- spaceOut '|' nxt
   pure $ l : comms ++ nxt' : dataBarsL ls'

imports :: [String] -> [String]
imports xs = let (a, b) = break (=="--+") xs in a ++ "" : loop (drop 1 b) where
   loop (x:l) | "import " `isPrefixOf` x
               , x' <- dropWhile isSpace $ drop 6 x
               = if "qualified" `isPrefixOf` x'
                  then warn "Use of qualified in multiplex import." skip
                  else
                     let (a, b) =
                           span (\y -> case y of c:_ -> isSpace c; _ -> True) l
                     in doImport (intercalate "\n" $ map decomment $ x' : a)
                           : loop b
              | __ = skip
              where skip = x : loop l
   loop _ = []

doImport :: String -> String
doImport blk = go 0 "" rest where
   (name, rest) =
      case break (\c -> c==',' || isSpace c) blk of
         (a, b) | '"':_ <- dropWhile isSpace a
                  , (b1, b2) <- break (==' ') $ dropWhile isSpace b
                     -> (a ++ " " ++ b1, b2)
         pair -> pair
   go :: Int -> String -> String -> String
   go p acc nx =
      let (nx1, nx2) = break (`elem` ",()") nx
          acc' = acc ++ nx1
      in case nx2 of
         '(':b          -> go (p+1) (acc' ++ "(") b
         ')':b | p > 0  -> go (p-1) (acc' ++ ")") b
         ',':b | p == 0 ->
            render acc' ++ (if all isSpace b then "" else ";" ++ go 0 "" b)
         c:b            -> go p (acc' ++ [c]) b
         _              -> render acc'
   render acc =
      "import "
         ++ (if "as" `isPrefixOf` dropWhile isSpace acc
               then "qualified " else "")
         ++ name ++ " " ++ acc

process :: String -> String 
process =
   unlines . imports . dataBarsL . commasL . commasR . dittoMarks
      . map (emptyGuard . tickedNums . untab) . lines

main = do
   args <- getArgs
   case args of
      ["--version"] -> putStrLn $ "hpre v" ++ showVersion version
      [nm, inf, outf] -> do
         file <- readFile inf
         writeFile outf
            $ "{-# LINE 1 \"" ++ nm ++ "\" #-}\n" ++ process file
      [] -> interact process
      _  -> abort "Usage: hpre [name infile outfile]"

