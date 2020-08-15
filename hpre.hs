import System.Environment
import Data.Char
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Maybe

import Debug.Trace

--  YMMV
tabWidth = 3 :: Int

{-# INLINE __ #-}
__ = True  -- second-best

warn :: String -> a -> a
warn = trace . ("Warning: " ++)

--  Yank up to end of quote.
--  TODO multiline quote support
skipQuote :: String -> (String, String)
skipQuote s = go "" s where
   go q []            = (q, [])
   go q ('\\':[])     = (q++'\\':[], [])
   go q ('"':s)       = (q++"\"", s)
   go q ('\\':'\\':s) = go (q++"\\\\") s
   go q ('\\':'"':s)  = go (q++"\\\"") s
   go q (c:s)         = go (q++c:[]) s

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
--  It should be removed eventually, as it can interfere with alignment.
tickedNums :: String -> String
tickedNums []      = []
tickedNums l@(x:m)
   | isDigit x =
      let (p1, p2) = span (\y -> isDigit y || isTick y) l
          p1' = filter (not.isTick) p1
                  ++ (reverse $ takeWhile isTick $ reverse p1) -- prob not needed
      in (if p1/=p1' then warn "Ticked numbers are deprecated." else id)
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
      pfx 
         | take 1 p2 `elem` ["=", "→"] || take 2 p2 == "->" =
            case length p1 of
               x | x<4  -> "True"
                 | __   -> c2 : "True" ++ drop 4 p1
         | __ = c2 : p1
emptyGuard (c:s) = c : emptyGuard s
emptyGuard _ = []

type DittoHist = [(Int, String)]

columnPragma :: Int -> String
columnPragma col = "{-#COLUMN " ++ show col ++ "#-}"

--  "Ditto marks" for repeated names in definitions.
--  TODO? allow mid-line names to be ditto'ed
dittoM :: String -> State DittoHist String
dittoM s = (sp1 ++) <$>
   case rest of
      '\'':'\'':x:rest' | isSpace x         -> doDitto 2 rest'
      c:x:rest' | c `elem` "”〃", isSpace x -> doDitto 1 rest'
      c:_ | isLower c                       -> do
         modify $ \hist ->
            (indent, takeWhile isNameChar rest)
               : dropWhile ((>= indent) . fst) hist
         pure rest
      _                                     -> pure rest
   where
      (sp1, rest) = span isSpace s
      indent = length sp1
      doDitto :: Int -> String -> State DittoHist String
      doDitto wid rest' = do
         valm <- lookup indent <$> get
         pure $ case valm of
            Just val ->
               let (sp2, rest'') = span isSpace rest' in
                  val ++
                  columnPragma (indent + wid + length sp2 + 2) ++
                  rest''
            _ -> error $ "Orphaned ditto mark:  " ++ s

dittoMarks :: [String] -> [String]
dittoMarks inp = flip evalState [] $ mapM dittoM inp

--  TODO can get messed up by quotes
decomment :: String -> String
decomment s = case s of
   '-':'-':_  -> []
   c:s'       -> c : decomment s'
   _          -> []

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
   ',' : l' <- pure $ dropWhile isSpace $ reverse $ decomment l
   nxt : _  <- pure $ snd $ spanCommentLines ls
   c : _    <- pure $ dropWhile isSpace nxt
   guard $ c `elem` "])}"
   pure $ reverse l'

commasL :: [String] -> [String]
commasL []     = []
commasL (l:ls) = fromMaybe (l : commasL ls) $ do
   c : _ <- pure $ dropWhile isSpace $ reverse $ decomment l
   guard $ c `elem` "{(["
   let (comms, rest) = spanCommentLines ls
   nxt : ls' <- pure rest
   nxt' <- spaceOut ',' nxt
   pure $ l : comms ++ nxt' : commasL ls'

dataBarsL :: [String] -> [String]
dataBarsL []    = []
dataBarsL (l:ls) = fromMaybe (l : dataBarsL ls) $ do
   guard $ "data " `isPrefixOf` l
   '=' : _ <- pure $ dropWhile isSpace $ reverse $ decomment l
   let (comms, rest) = spanCommentLines ls
   nxt : ls' <- pure rest
   nxt' <- spaceOut '|' nxt
   pure $ l : comms ++ nxt' : dataBarsL ls'

imports :: [String] -> [String]
imports [] = []
imports (x:l') | x == "--+" = "" : loop l'
               | __         = x : imports l'
   where
   loop [] = []
   loop (x:l) | "import " `isPrefixOf` x
               , x' <- dropWhile isSpace $ drop 6 x
               = if "qualified" `isPrefixOf` x'
                  then warn "Use of qualified in multiplex import." $ continue
                  else
                     let (a, b) =
                           span (\y -> case y of c:_ -> isSpace c; _ -> True) l
                     in doBlock (concat $ intersperse "\n" $ x' : a) : loop b
              | __ = continue
              where continue = x : loop l
   doBlock blk = go 0 "" rest where
      (mod, rest) =
         case break (\c -> c==',' || isSpace c) blk of
            (a, b) | '"':_ <- dropWhile isSpace a
                     , (b1, b2) <- break (==' ') $ dropWhile isSpace b
                        -> (a ++ " " ++ b1, b2)
            pair -> pair
      go p acc nx =
         let (a, b) = break (\c -> c `elem` ",()") nx
             acc' = acc ++ a
         in case b of
            '(':b          -> go (p+1) (acc' ++ "(") b
            ')':b | p > 0  -> go (p-1) (acc' ++ ")") b
            ',':b | p == 0 ->
               mkimport acc' ++ (if all isSpace b then "" else ";" ++ go 0 "" b)
            c:b            -> go p (acc' ++ [c]) b
            _              -> mkimport acc'
      mkimport acc =
         "import "
            ++ (if "as" `isPrefixOf` dropWhile isSpace acc
                  then "qualified " else "")
            ++ mod ++ " " ++ acc

process :: String -> String 
process =
   unlines . imports . dataBarsL . commasL . commasR . dittoMarks .
   map (emptyGuard . tickedNums . untab) . lines

main = do
   args <- getArgs
   case args of
      [nm, inf, outf] -> do
         file <- readFile inf
         writeFile outf
            $ "{-# LINE 1 \"" ++ nm ++ "\" #-}\n" ++ process file
      [] -> interact process
      _  -> error "Usage: hpre [name infile outfile]"

