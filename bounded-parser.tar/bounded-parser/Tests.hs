module Tests where

import Calc

import Control.Applicative (Alternative, liftA2, (<|>))
import Test.QuickCheck

--
--
-- Tests
--
--


checkEq :: (Eq a, Show a) => a -> a -> String
checkEq got expected
    | expected == got = "PASS"
    | otherwise       = "FAIL Expected: " ++ show expected ++ " Got: " ++ show got

-- These are just basic tests: we do not guarantee that they cover everything!
-- You can add your own tests here.

testEval :: IO ()
testEval = do
  putStr "add: "
  putStrLn $ checkEq (evalCExpr (MkCExpr (MkCFact (Num 1) EndFact)
                                    (Add (MkCFact (Num 1) EndFact) EndExpr)))
                     2

  putStr "sub: "
  putStrLn $ checkEq (evalCExpr (MkCExpr (MkCFact (Num 1) EndFact)
                                    (Sub (MkCFact (Num 1) EndFact) EndExpr)))
                     0

  putStr "mul: "
  putStrLn $ checkEq (evalCExpr (MkCExpr (MkCFact (Num 2) (Mul (Num 2) EndFact)) EndExpr))
                     4

  putStr "mod: "
  putStrLn $ checkEq (evalCExpr (MkCExpr (MkCFact (Num 5) (Mod (Num 2) EndFact)) EndExpr))
                     1


testParse :: IO ()
testParse = do
  putStr "satisfy: "
  putStrLn $ checkEq (runParser (satisfy (== 'x')) "x")
                     (Consumed $ ParseOk 'x' "")

  putStr "not satisfy: "
  putStrLn $ checkEq (runParser (satisfy (/= 'x')) "x")
                     (NotConsumed $ ParseErr)

  putStr "bind 1: "
  putStrLn $ checkEq (runParser ((return ()) >>= (\_ -> return ())) "x")
                     (NotConsumed $ ParseOk () "x")

  putStr "bind 2: "
  putStrLn $ checkEq (runParser ((satisfy (== 'x')) >>= (\_ -> return ())) "x")
                     (Consumed $ ParseOk () "")

  putStr "bind 3: "
  putStrLn $ checkEq (runParser ((return ()) >>= (\_ -> satisfy (== 'x'))) "x")
                     (Consumed $ ParseOk 'x' "")

  putStr "choice 1: "
  putStrLn $ checkEq (runParser (satisfy (== 'x') <|> satisfy (/= 'x')) "x")
                     (Consumed $ ParseOk 'x' "")

  putStr "choice 2: "
  putStrLn $ checkEq (runParser (satisfy (/= 'x') <|> satisfy (== 'x')) "x")
                     (Consumed $ ParseOk 'x' "")

  putStr "choice 3: "
  putStrLn $ checkEq (runParser (satisfy (== 'x') >> satisfy (== 'y') <|> satisfy (== 'x')) "x")
                     (Consumed $ ParseErr)


--
--
-- Expression generator
--
-- For more advanced tests, we can randomly generate expressions. Here, we set
-- up a QuickCheck generator for `CExpr`. You can use this generator with
-- QuickCheck properties to do property-based testing of your parser (see the
-- properties below).
--
-- To run the QuickCheck tests, use `cabal v2-repl` and then type `:l
-- Tests` to load the module. Then, you can do
--
-- `quickCheck printParse`
--
-- to run each test 100 times.
--
--

-- This controls the size of terms produced (don't turn it up too high!)
genSize :: Int
genSize = 5

--
--
-- An expression generator
--
--

genCExpr :: Int -> Gen CExpr
genCExpr d = MkCExpr <$> (genCFact d) <*> (genCExprR d)

genCExprR :: Int -> Gen CExprR
genCExprR 0 = pure EndExpr
genCExprR d = oneof [ pure EndExpr
                    , Add <$> (genCFact (d - 1)) <*> (genCExprR (d - 1))
                    , Sub <$> (genCFact (d - 1)) <*> (genCExprR (d - 1))
                    ]

genCFact :: Int -> Gen CFact
genCFact d = MkCFact <$> (genCTerm d) <*> (genCFactR d)

genCFactR :: Int -> Gen CFactR
genCFactR 0 = pure EndFact
genCFactR d = oneof [ pure EndFact
                    , Mul <$> (genCTerm (d - 1)) <*> (genCFactR (d - 1))
                    , Mod <$> (genCTerm (d - 1)) <*> (genCFactR (d - 1))
                    ]

genCTerm :: Int -> Gen CTerm
genCTerm 0 = Num <$> choose(0, 20)
genCTerm d = oneof [ Num <$> choose(0,20)
                   , Parens <$> (genCExpr (d - 1))
                   ]

--
--
-- Quickcheck property: pretty-print/parse
--
--

printParse :: Property
printParse = forAll (genCExpr genSize)
  (\e -> Consumed (ParseOk e "") == runParser parseCExpr (tokenize . ppCExpr $ e))
