module Main where

import System.Environment
import Tokens
import Grammar

main :: IO ()
main = do line <- getLine
          print (parseCalc (alexScanTokens line))
          main

          



