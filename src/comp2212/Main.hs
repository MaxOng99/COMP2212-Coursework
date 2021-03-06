module Main where

import System.Environment
import Tokens
import Grammar
import SplEvaluator
import TypeChecker

main :: IO ()
main = do args <- getArgs
          sourceCode <- readFile (args !! 0)
          inputStream <- getContents
          let parsedProgram = parseCalc (alexScanTokens sourceCode)
          let typeCheckingComplete = initiateTypeChecking parsedProgram
          if (typeCheckingComplete) then 
              do let inputSequence = generateInputSequences $ splitCol $ splitRow inputStream
                 result <- evalLoop (convertToState parsedProgram inputSequence)
                 putStr (formatOutput $ convertToIntList result)
          else putStr ("Type Checker failed")

          

splitRow :: String -> [String]
splitRow xs = lines xs

splitCol :: [String] -> [[String]]
splitCol [] = []
splitCol (x:xs) = words x:splitCol xs

generateInputSequences :: [[String]] -> [[Int]]
generateInputSequences xss
    | not $ checkAllEmpty xss = [(read x) :: Int | (x:xs) <- xss]:generateInputSequences (map tail xss)
    | otherwise = []

checkAllEmpty :: [[a]] -> Bool
checkAllEmpty [] = True
checkAllEmpty (x:xs)
    | length x /= 0 = False
    | otherwise = checkAllEmpty xs

formatOutput :: [Int] -> String
formatOutput [] = []
formatOutput [x] = show x
formatOutput (x:xs) = formatOutput xs ++ "\n" ++ show x

convertToIntList :: Exp -> [Int]
convertToIntList (List xs) = (read xs) :: [Int]

