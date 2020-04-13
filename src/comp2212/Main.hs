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
          --putStrLn ("Parsed program is " ++ show parsedProgram ++ "\n")
          --putStrLn ("Initiating type checking...")
          let typeCheckingComplete = initiateTypeChecking parsedProgram
          --putStrLn ("Type checking success -- " ++ show typeCheckingComplete)
          let inputSequence = generateInputSequences $ splitCol $ splitRow inputStream
          result <- evalLoop (convertToState parsedProgram inputSequence)
          putStrLn (formatOutput $ convertToIntList result)

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
formatOutput (x:xs) = formatOutput xs ++ "\n" ++ show x

convertToIntList :: Exp -> [Int]
convertToIntList (List xs) = (read xs) :: [Int]

