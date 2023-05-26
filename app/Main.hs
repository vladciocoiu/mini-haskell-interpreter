module Main (main) where

import System.IO
import REPLCommand
import Parsing
import Show
import Eval
import Sugar
import Program (Environment, normalizeEnv, program, programEnv)
import Data.Map (empty)
import Lab2 (parseFromFile)

main :: IO ()
main = execute Data.Map.empty
    
execute :: Environment -> IO ()
execute env = do
    putStr "miniHaskell> "
    hFlush stdout
    s <- getLine
    case testParse replCommand s of
          Nothing -> putStrLn "Cannot parse command" >> execute env
          Just Quit -> return ()
          Just (Load file) -> do
                str <- parseFromFile program file
                case str of
                    Right s -> let newEnv = programEnv s in execute newEnv
                    Left err -> putStrLn "Error: cannot read file" >> execute env
          Just (Eval es) ->
            case testParse exprParser es of
              Nothing -> putStrLn "Error: cannot parse expression" >> execute env 
              Just e -> putStrLn (showExp (sugarExp (normalizeEnv env (desugarExp e)))) >> execute env
