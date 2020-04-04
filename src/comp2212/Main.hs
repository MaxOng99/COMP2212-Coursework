module Main where

import System.Environment
import Tokens
import Grammar
import SplEvaluator

main :: IO ()
main = do args <- getArgs
          content <- readFile (head args)
          let parsedProgram = parseCalc (alexScanTokens content)
          print (parsedProgram)
          let result = evalLoop (convertToState parsedProgram inputSequence)