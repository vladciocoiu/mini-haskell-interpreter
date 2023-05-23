module Main (main) where

import System.IO
import REPLCommand
import Parsing
import Show
import Eval
import Sugar

main :: IO ()
main = do
    putStr "miniHaskell> "
    hFlush stdout
    s <- getLine
    case testParse replCommand s of
          Nothing -> putStrLn "Cannot parse command" >> main
          Just Quit -> return ()
          Just (Load _) -> putStrLn "Not implemented" >> main
          Just (Eval es) ->
            case testParse exprParser es of
              Nothing -> putStrLn "Error: cannot parse expression" >> main 
              Just e -> putStrLn (showExp (sugarExp (normalize (desugarExp e)))) >> main
