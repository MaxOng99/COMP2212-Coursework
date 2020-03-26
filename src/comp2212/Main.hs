module Main where

import System.Environment
import Tokens
import Grammar

main :: IO ()
main = do args <- getArgs
          content <- readFile (head args)
          print (parseCalc (alexScanTokens content))


          



