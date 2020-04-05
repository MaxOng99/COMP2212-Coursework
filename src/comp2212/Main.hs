module Main where

import System.Environment
import Tokens
import Grammar
import SplEvaluator

main :: IO ()
main = do args <- getArgs
          sourceCode <- readFile (args !! 0)
          inputStream <- readFile (args !! 1)
          let parsedProgram = parseCalc (alexScanTokens sourceCode)
          let result = evalLoop (convertToState parsedProgram inputSequence) where
              inputSequence = generateInputSequences $ splitCol $ splitRow inputStream
          print (result)

splitRow :: String -> [String]
splitRow xs = lines xs

splitCol :: [String] -> [[String]]
splitCol [] = []
splitCol (x:xs) = words x:splitCol xs

generateInputSequences :: [[String]] -> [[Int]]
generateInputSequences [] = []
generateInputSequences xss = [(read x) :: Int | (x:xs) <- xss]:generateInputSequences (map tail xss) 
