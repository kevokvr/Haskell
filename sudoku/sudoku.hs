-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Kevin Valenzuela
-- Date: October 25, 2019
-- NOTE: The sudoku5.txt will take about 2 minutes to return a solution :)

import System.Environment
import System.IO
import Data.List


type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- name: toInt
toInt :: [Char] -> Int
toInt s = read s :: Int

-- name: toIntList
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

--helper method to seperate lists into 3 parts
sepInto3 :: [a] -> [[a]]
sepInto3 (a:b:c:lst) = [a,b,c] : sepInto3 lst
sepInto3 []         = []

-- Helper Method
allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

printBoard :: Show a => [a] -> IO()
printBoard = putStrLn . unlines . map show

-- ***** GETTER FUNCTIONS *****

-- TODO #1
getBoard :: [Char] -> Board
getBoard s = map toIntList fileLines
  where
    fileLines = lines s

-- TODO #2
getNRows :: Board -> Int
getNRows b = length b

-- TODO #3
getNCols :: Board -> Int
getNCols board
    | all (==head lst) lst = head lst
    | otherwise = 0
    where lst = map length board

-- TODO #4
-- name: getBox
getBox :: Board -> Int -> Int -> Sequence
getBox board x y = seq where
    seq = s!!x!!y
    s = sepInto3 grid
    grid = boxes t
    boxes = map concat . sepInto3 . concat . transpose . map sepInto3
    t = transpose board

-- TODO #5
getEmptySpot :: Board -> (Int, Int)
getEmptySpot board = head lst
    where lst = [(row, col) | row <- [0..8], col <- [0..8], board!!row!!col == 0 ]

-- ***** PREDICATE FUNCTIONS *****

-- TODO #6
-- hint: use getNRows and getNCols
isGridValid :: Board -> Bool
isGridValid board
    | getNRows board == getNCols board = True
    | otherwise = False

-- TODO #7
isSequenceValid :: Sequence -> Bool
isSequenceValid seq = allUnique lst
    where lst = [x | x <- seq, x /= 0]

-- TODO #8
areRowsValid :: Board -> Bool
areRowsValid board
    | False `notElem` lst = True
    | otherwise = False
    where lst = [ isSequenceValid x | x <- board ]

-- TODO #9
areColsValid :: Board -> Bool
areColsValid board = areRowsValid tpose
    where tpose = transpose board

-- TODO #10
areBoxesValid :: Board -> Bool
areBoxesValid board
    | False `notElem` lst = True
    | otherwise = False
    where 
    lt = [getBox board x y | x <- [0..2], y <- [0..2]]
    lst = [ isSequenceValid var | var <- lt]

-- TODO #11
isValid :: Board -> Bool
isValid board = and [
    isGridValid board == True
    ,areRowsValid board == True
    ,areColsValid board == True
    ,areBoxesValid board == True ]

-- TODO #12
isCompleted board 
    | 0 `notElem` lst = True
    | otherwise = False
    where lst = [ board !!x!!y | x <- [0..8], y <- [0..8]] 

-- TODO #13
isSolved :: Board -> Bool
isSolved board = and [
    isValid board == True
    ,isCompleted board == True ]

-- ***** SETTER FUNCTIONS *****

-- TODO #14
setRowAt :: Sequence -> Int -> Int -> Sequence
setRowAt seq index newValue
    | seq !!index == 0 = newSeq
    | otherwise = seq
    where 
        newSeq = take index seq ++ [newValue] ++ drop (index + 1) seq
        
-- TODO #15
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt board indexOfBoard indexOfSeq newValue
    | board !!indexOfBoard!!indexOfSeq == 0 = newBoard
    | otherwise = board
    where
        modifyThis = board !!indexOfBoard
        newSeq = setRowAt modifyThis indexOfSeq newValue
        newBoard = take indexOfBoard board ++ [newSeq] ++ drop (indexOfBoard + 1) board


-- TODO #16
buildChoices :: Board -> Int -> Int -> [Board]
buildChoices board boardIndex seqIndex = boards
        where 
            boards = [setBoardAt board boardIndex seqIndex x | x <- [1..9] ]
-- Solve ! --
solve :: Board -> [Board]
solve board
  | isSolved board = [board]
  | isCompleted board = [[[]]]
  | not (isValid board) = [[[]]]
  | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
    where
      emptySpot = getEmptySpot board
      i = fst emptySpot
      j = snd emptySpot

main = do
  -- TODO #17: validate the command-line and get the file name containing the board
  putStr "Enter file name: "
  name <- getLine
  file <- readFile name
  -- TODO #19: create a board from the string board (hint: use getBoard)
  let bord = getBoard file
  -- TODO #20: use solve to find the solutions, disconsidering the ones that are [[]]
  let sols = [x | x <- solve bord, x /= [[]]]
  -- TODO #21: print the solutions found
  printBoard (concat sols)
  print "Done!, by Kevin Valenzuela"