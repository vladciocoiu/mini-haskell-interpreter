module Main (main) where

import REPLCommand
import Parsing
import Show

main :: IO ()
main = do
        s <- getLine
        let x = testParse replCommand s in
            case x of
                Nothing -> putStrLn  "Cannot parse" >> main
                Just Quit -> putStrLn "Quit"
                Just (Load s) -> main
                Just (Eval s) -> 
                    let ce = testParse expr s in
                        case ce of
                            Nothing -> putStrLn "Cannot parse" >> main
                            Just ce -> (putStrLn . showExp $ ce) >> main

