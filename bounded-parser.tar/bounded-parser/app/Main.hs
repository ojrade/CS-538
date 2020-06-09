module Main where

import Calc

import System.IO

main :: IO ()
main = do
  putStrLn "Welcome to the Calc REPL! (Press Ctrl-D to quit.)"
  repl

--
--
-- Main REPL
--
--

repl :: IO ()
repl = do
  putStr "CalcREPL> "
  hFlush stdout
  input <- getLine
  case runParser parseCExpr (tokenize input) of
    Consumed (ParseOk cexpr _) ->
      putStrLn $ "Result: " ++ show (evalCExpr cexpr)
    NotConsumed (ParseOk cexpr _) ->
      putStrLn $ "Result: " ++ show (evalCExpr cexpr)
    _ ->
      putStrLn $ "Parse error on input: " ++ input
  repl
