module Calc where

import Control.Applicative (Alternative)
import Control.Monad
import Control.Monad.Combinators
import Data.Char (isDigit, digitToInt, isSpace)

--
--
-- Abstract syntax tree
--
--

data CExpr = MkCExpr CFact CExprR
            deriving (Eq, Show)

data CExprR = EndExpr
            | Add CFact CExprR
            | Sub CFact CExprR
            deriving (Eq, Show)

data CFact = MkCFact CTerm CFactR
           deriving (Eq, Show)

data CFactR = EndFact
            | Mul CTerm CFactR
            | Mod CTerm CFactR
            deriving (Eq, Show)

data CTerm = Num Int
           | Parens CExpr
           deriving (Eq, Show)

--
--
-- Pretty printer
--
--

ppCExpr :: CExpr -> String
ppCExpr (MkCExpr cf cex) = ppCFact cf ++ " " ++ ppCExprR cex

ppCExprR :: CExprR -> String
ppCExprR EndExpr = ""
ppCExprR (Add cf cex) = "+ " ++ ppCFact cf ++ " "++ ppCExprR cex
ppCExprR (Sub cf cex) = "- " ++ ppCFact cf ++ " " ++ppCExprR cex

ppCFact :: CFact -> String
ppCFact (MkCFact ct cfr) = ppCTerm ct ++ " " ++ ppCFactR cfr

ppCFactR :: CFactR -> String
ppCFactR EndFact = ""
ppCFactR (Mul ct cfr) = "* " ++ ppCTerm ct ++ " " ++ ppCFactR cfr
ppCFactR (Mod ct cfr) = "% " ++ ppCTerm ct ++ " " ++ ppCFactR cfr

ppCTerm :: CTerm -> String
ppCTerm (Num i) = show i
ppCTerm (Parens cex) = "(" ++ ppCExpr cex ++ ")"

--
--
-- Evaluator
--
--

evalCExpr :: CExpr -> Int
evalCExpr (MkCExpr cf cer) = evalCExprR (evalCFact cf) cer

evalCExprR :: Int -> CExprR -> Int
evalCExprR i EndExpr = i
evalCExprR i (Add cf cer) = i + (evalCExprR (evalCFact cf) cer)
evalCExprR i (Sub cf cer) = i - (evalCExprR (evalCFact cf) cer)

evalCFact :: CFact -> Int
evalCFact (MkCFact ct cfr) = (evalCFactR (evalCTerm ct) cfr)

evalCFactR :: Int -> CFactR -> Int
evalCFactR i EndFact = i
evalCFactR i (Mul ct cfr) = i * (evalCFactR (evalCTerm ct) cfr)
evalCFactR i (Mod ct cfr) = mod i (evalCFactR (evalCTerm ct) cfr)

evalCTerm :: CTerm -> Int
evalCTerm (Num i) = i
evalCTerm (Parens cex) = evalCExpr cex

--
--
-- Parser
--
--

data PResult a = ParseOk a String
               | ParseErr
               deriving (Eq, Show)

data BPResult a = Consumed (PResult a)
                | NotConsumed (PResult a)
                deriving (Eq, Show)

newtype BParser a = MkParser { runParser :: String -> BPResult a }

instance Monad BParser where
  return = pReturn
  (>>=)  = pBind

pReturn :: a -> BParser a
pReturn x = MkParser $ \st -> NotConsumed (ParseOk x st)

pBind :: BParser a -> (a -> BParser b) -> BParser b
pBind p f = MkParser $ \st ->
                 case (runParser p st) of
                   (Consumed (ParseOk a st)) -> case runParser (f a) st of
                                                  (Consumed pr) -> Consumed (pr)
                                                  (NotConsumed pr) -> Consumed (pr)
                   (NotConsumed (ParseOk a st)) -> runParser (f a) st
                   (Consumed (ParseErr)) -> Consumed (ParseErr)
                   (NotConsumed (ParseErr)) -> NotConsumed (ParseErr) 

instance Alternative BParser where
  empty = pZero
  (<|>) = pPlus

pZero :: BParser a
pZero = MkParser $ \st -> NotConsumed ParseErr

pPlus :: BParser a -> BParser a -> BParser a
pPlus p1 p2 = MkParser $ \st ->
                   case (runParser p1 st) of
                     (NotConsumed ParseErr) -> runParser p2 st
                     _ -> runParser p1 st


instance Functor BParser where
  fmap = pMap

pMap :: (a -> b) -> BParser a -> BParser b
pMap f p = do { x <- p ; return $ f x }

instance Applicative BParser where
  pure  = pReturn
  (<*>) = pApp
  p1 *> p2 = do { p1 ; p2 }
  p1 <* p2 = do { x <- p1 ; p2 ; return x }

pApp :: BParser (a -> b) -> BParser a -> BParser b
pApp pf p = do f <- pf
               f <$> p

instance MonadPlus BParser where
  mzero = pZero
  mplus = (<|>)

satisfy :: (Char -> Bool) -> BParser Char
satisfy predicate = MkParser $ \st ->
                       case st of
                         [] -> NotConsumed (ParseErr)
                         (c:cs) -> if (predicate c)
                                    then (Consumed (ParseOk c cs))
                                    else (NotConsumed (ParseErr))

digit :: BParser Int
digit = digitToInt <$> satisfy isDigit

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\cur new -> 10 * cur + new) 0

int :: BParser Int
int = digitsToInt <$> some digit

--
--
-- Calc parser
--
--
{-
parsePlus :: Globals -> Context -> Parser RExpr
parsePlus gctx ctx = do 
                      symbol "("
                      symbol "+"
                      e1 <- parseRExpr' gctx ctx
                      spaces 
                      e2 <- parseRExpr' gctx ctx
                      optSpaces
                      chunk ")"
                      return (Plus e1 e2)
-}
charP :: Char -> BParser Char
charP c = satisfy (== c)

tokenize :: String -> String
tokenize = filter (not . isSpace)

parseCExpr :: BParser CExpr
parseCExpr = (MkCExpr <$> parseCFact <*> parseCExprR)

parseCExprR :: BParser CExprR
parseCExprR = pure EndExpr 
          <|> Add <$> parseCFact <*> parseCExprR
          <|> Sub <$> parseCFact <*> parseCExprR
               

parseCFact :: BParser CFact
parseCFact = MkCFact <$> parseCTerm <*> parseCFactR

parseCFactR :: BParser CFactR
parseCFactR = pure EndFact
          <|> Mul <$> parseCTerm <*> parseCFactR
          <|> Mod <$> parseCTerm <*> parseCFactR

parseCTerm :: BParser CTerm
parseCTerm = Num <$> int
         <|> Parens <$> parseCExpr
