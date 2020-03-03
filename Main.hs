--
-- EPITECH PROJECT, 2020
-- FUN_wolfram_2019
-- File description:
-- main
--

import Data.Char
import System.IO
import System.Environment
import System.Exit

isStringDigit :: [Char] -> Bool --verifie si la chaine de caractéres est uniquement composé de nombre
isStringDigit [] = True
isStringDigit (x:xs)
    | x == '-' = isStringDigit(xs)
    | isDigit x == False = False
    | otherwise = isStringDigit(xs)

parsing :: [String] -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int) --Function de Parsing
parsing [] array = array
parsing (x:[]) (rule, start, lines, window, move, error) = (rule, start, lines, window, move, 84)
parsing ("--rule":x:xs) (rule, start, lines, window, move, error)
    | isStringDigit x && rule == 0 && (read(x)::Int) >= 0 && (read(x)::Int) <= 225 = parsing(xs) (read (x)::Int, start, lines, window, move, error)
parsing ("--start":x:xs) (rule, start, lines, window, move, error)
    | isStringDigit x && start == 0 = parsing(xs) (rule, read(x)::Int, lines, window, move, error)
parsing ("--lines":x:xs) (rule, start, lines, window, move, error)
    | isStringDigit x && lines == -1 && (read(x)::Int) >= 0 = parsing(xs) (rule, start, read(x)::Int, window, move, error)
parsing ("--window":x:xs) (rule, start, lines, window, move, error)
    | isStringDigit x && window == 80 = parsing(xs) (rule, start, lines, read(x)::Int, move, error)
parsing ("--move":x:xs) (rule, start, lines, window, move, error)
    | isStringDigit x && move == 0 = parsing(xs) (rule, start, lines, window, read(x)::Int, error)
parsing(_:x:xs) (rule, start, lines, window, move, error) = (rule, start, lines, window, move, 84)

printArray :: (Int, Int) -> [Int] -> IO () --Affiche la ligne reçue en paramétre
printArray (0,_) _ = putStrLn ""
printArray (window, move) line
    | move < 0 = printArray (window, (move+1)) (line ++ [0,0])
    | move > 0 = printArray (window, (move-1)) (0:0:line)
printArray (window, move) line
    | length line < window = printArray (window, move) ([0] ++ line ++ [0])
    | length line > window + 1 = printArray (window, move) (init $ tail line)
    | length line > window = printArray (window, move) (init line)
printArray (window,0) (0:xs) = putStr " " >> printArray ((window-1),0) xs
printArray (window,0) (1:xs) = putStr "*" >> printArray ((window-1),0) xs

createLine30 :: [Int] -> [Int] -> [Int] --Créer une ligne de la régle 30 : window, old_line, new_line
createLine30 [] [] = [1]

createLine30 [] new_line = new_line
createLine30 (_:[]) new_line = new_line
createLine30 (_:_:[]) new_line = new_line
createLine30 (1:0:0:xs) new_line = createLine30 (0:0:xs) (new_line ++ [1])
createLine30 (0:1:1:xs) new_line = createLine30 (1:1:xs) (new_line ++ [1])
createLine30 (0:1:0:xs) new_line = createLine30 (1:0:xs) (new_line ++ [1])
createLine30 (0:0:1:xs) new_line = createLine30 (0:1:xs) (new_line ++ [1])
createLine30 (x:y:z:xs) new_line = createLine30 (y:z:xs) (new_line ++ [0])

createLine90 :: [Int] -> [Int] -> [Int] --Créer une ligne de la régle 90 : window, old_line, new_line
createLine90 [] [] = [1]
createLine90 [] new_line = new_line
createLine90 (_:[]) new_line = new_line
createLine90 (_:_:[]) new_line = new_line
createLine90 (1:1:0:xs) new_line = createLine90 (1:0:xs) (new_line ++ [1])
createLine90 (1:0:0:xs) new_line = createLine90 (0:0:xs) (new_line ++ [1])
createLine90 (0:1:1:xs) new_line = createLine90 (1:1:xs) (new_line ++ [1])
createLine90 (0:0:1:xs) new_line = createLine90 (0:1:xs) (new_line ++ [1])
createLine90 (x:y:z:xs) new_line = createLine90 (y:z:xs) (new_line ++ [0])

createLine110 :: [Int] -> [Int] -> [Int] --Créer une ligne de la régle 90 : window, old_line, new_line
createLine110 [] [] = [1]
createLine110 [] new_line = new_line
createLine110 (_:[]) new_line = new_line
createLine110 (_:_:[]) new_line = new_line
createLine110 (1:1:0:xs) new_line = createLine110 (1:0:xs) (new_line ++ [1])
createLine110 (1:0:1:xs) new_line = createLine110 (0:1:xs) (new_line ++ [1])
createLine110 (0:1:1:xs) new_line = createLine110 (1:1:xs) (new_line ++ [1])
createLine110 (0:1:0:xs) new_line = createLine110 (1:0:xs) (new_line ++ [1])
createLine110 (0:0:1:xs) new_line = createLine110 (0:1:xs) (new_line ++ [1])
createLine110 (x:y:z:xs) new_line = createLine110 (y:z:xs) (new_line ++ [0])

createLine :: [Int] -> [Int] -> Int -> [Int]
createLine old_line new_line 30 = createLine30 old_line new_line
createLine old_line new_line 90 = createLine90 old_line new_line
createLine old_line new_line 110 = createLine110 old_line new_line


rule :: (Int, Int, Int, Int) -> [Int] -> Int -> IO () --Régle 30 : (start, lines, window, move) -> old_line
rule (start, 0, window, move) _ _ = putStr ""
rule (start, lines, window, move) old_line rules = do
    let new_line = createLine ([0,0] ++ old_line ++ [0,0]) [] rules
    if (start == 0) then do
        printArray (window, move) old_line
        rule (start, lines - 1, window, move) new_line rules
    else
        rule (start - 1, lines, window, move) new_line rules

main :: IO() --fonction principal
main = do
    args <- getArgs
    let (rules, start, lines, window, move, error) = parsing args (0, 0, -1, 80, 0, 0)
    if error == 84 || args == [] then
        exitWith (ExitFailure 84)
    else if (rules == 30 || rules == 90 || rules == 110) then
        rule (start, lines, window, move) [1] rules 
    else
        exitWith (ExitFailure 84)
