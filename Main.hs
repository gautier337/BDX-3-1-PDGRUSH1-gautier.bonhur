{-
-- EPITECH PROJECT, 2023
-- Pushswap checker
-- File description:
-- Rush 1
-}

import System.Environment
import Data.List
import System.Exit

sa :: ([Int], [Int]) -> ([Int], [Int])
sa ([], l_b) = ([], l_b)
sa (e1:e2:l_a, l_b) = (e2:e1:l_a, l_b)

sb :: ([Int], [Int]) -> ([Int], [Int])
sb (l_a, []) = (l_a, [])
sb (l_a, e1:e2:l_b) = (l_a, e2:e1:l_b)

sc :: ([Int], [Int]) -> ([Int], [Int])
sc ([], []) = ([], [])
sc (l1:l2:l_a, l3:l4:l_b) = (l2:l1:l_a, l4:l3:l_b)

pa :: ([Int], [Int]) -> ([Int], [Int])
pa (l_a, []) = (l_a, [])
pa (l_a, e:l_b) = (e:l_a, l_b)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], l_b) = ([], l_b)
pb (e:l_a, l_b) = (l_a, e:l_b)

ra :: ([Int], [Int]) -> ([Int], [Int])
ra ([], l_b) = ([], l_b)
ra (l_a, l_b) = (tail l_a <> [head l_a], l_b)

rb :: ([Int], [Int]) -> ([Int], [Int])
rb (l_a, []) = (l_a, [])
rb (l_a, l_b) = (l_a, tail l_b <> [head l_b])

rr :: ([Int], [Int]) -> ([Int], [Int])
rr ([], []) = ([], [])
rr (l_a, []) = (l_a, [])
rr ([], l_b) = ([], l_b)
rr (l_a, l_b) = (tail l_a <> [head l_a], tail l_b <> [head l_b])

rra :: ([Int], [Int]) -> ([Int], [Int])
rra ([], l_b) = ([], l_b)
rra (l_a, l_b) = (last l_a : init l_a, l_b)

rrb :: ([Int], [Int]) -> ([Int], [Int])
rrb (l_a, []) = (l_a, [])
rrb (l_a, l_b) = (l_a, last l_b : init l_b)

rrr :: ([Int], [Int]) -> ([Int], [Int])
rrr ([], []) = ([], [])
rrr (l_a, []) = (l_a, [])
rrr ([], l_b) = ([], l_b)
rrr (l_a, l_b) = (last l_a : init l_a, last l_b : init l_b)

applyOperationsTwo :: [String] -> ([Int], [Int]) -> ([Int], [Int])
applyOperationsTwo [] (l_a, l_b) = (l_a, l_b)
applyOperationsTwo (op:ops) (l_a, l_b) = case op of
    "ra" -> applyOperations ops (ra (l_a, l_b))
    "rb" -> applyOperations ops (rb (l_a, l_b))
    "rr" -> applyOperations ops (rr (l_a, l_b))
    "rra" -> applyOperations ops (rra (l_a, l_b))
    "rrb" -> applyOperations ops (rrb (l_a, l_b))
    "rrr" -> applyOperations ops (rrr (l_a, l_b))
    _ -> (l_a, l_b)

applyOperations :: [String] -> ([Int], [Int]) -> ([Int], [Int])
applyOperations [] (l_a, l_b) = (l_a, l_b)
applyOperations (op:ops) (l_a, l_b) = case op of
    "sa" -> applyOperations ops (sa (l_a, l_b))
    "sb" -> applyOperations ops (sb (l_a, l_b))
    "sc" -> applyOperations ops (sc (l_a, l_b))
    "pa" -> applyOperations ops (pa (l_a, l_b))
    "pb" -> applyOperations ops (pb (l_a, l_b))
    _ -> applyOperationsTwo (op:ops) (l_a, l_b)

checkOpExistsTwo :: [String] -> Bool
checkOpExistsTwo (op:ops) = case op of
    "ra" -> checkOpExists ops
    "rb" -> checkOpExists ops
    "rr" -> checkOpExists ops
    "rra" -> checkOpExists ops
    "rrb" -> checkOpExists ops
    "rrr" -> checkOpExists ops
    _ -> False

checkOpExists :: [String] -> Bool
checkOpExists [] = True
checkOpExists (op:ops) = case op of
    "sa" -> checkOpExists ops
    "sb" -> checkOpExists ops
    "sc" -> checkOpExists ops
    "pa" -> checkOpExists ops
    "pb" -> checkOpExists ops
    _ -> checkOpExistsTwo (op:ops)

readIntList :: IO [Int]
readIntList = do
    args <- getArgs
    return $ map read args

readOperations :: IO [String]
readOperations = do
    line <- getLine
    return $ words line

checkConditons :: ([Int], [Int]) -> [String] -> IO ()
checkConditons (l_a, l_b) operations
    | checkOpExists operations && l_a == sort l_a && l_b == [] = putStrLn "OK"
    | checkOpExists operations = putStrLn ("KO: " <> show (l_a, l_b))
    | otherwise = putStrLn "Invalid input" >> exitWith (ExitFailure 84)

main :: IO ()
main = do
    intList <- readIntList
    operations <- readOperations
    let (l_a, l_b) = applyOperations operations (intList, [])
    checkConditons (l_a, l_b) operations
