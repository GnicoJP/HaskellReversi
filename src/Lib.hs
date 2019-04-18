module Lib
    ( someFunc
    ) where

import Control.Monad
import Data.Array.IO
import System.IO(stdout, hFlush)

-- Defining type for field.
type Field = IOArray (Int, Int) FieldState

-- Field State Enum
data FieldState = FieldStateMan | FieldStateEnemy | FieldStateWall | FieldStateNone
        deriving Eq
-- Define instance for FieldState.
-- In Java, this is equals to
--   interface Show {}
--   class FieldState implements Show {
--     @override
--     public string show(FieldState fs) { ... }
--   }
instance Show FieldState where
    show FieldStateMan = "A"
    show FieldStateEnemy = "B"
    show FieldStateWall = "O"
    show FieldStateNone = " "

-- Game State
-- 今の所フィールド以外に何もデータを保持しないので意味がない＞＜
data ReversiState =
      ReversiStateMan Field
    | ReversiStateEnemy Field

-- Initialize State of game.
initState :: IO ReversiState
initState = do
        -- set none and wall
        ary <- (newListArray ((0,0), (9, 9)) listtoary)
        -- put center
        writeArray ary (4,4) FieldStateMan
        writeArray ary (5,5) FieldStateMan
        writeArray ary (4,5) FieldStateEnemy
        writeArray ary (5,4) FieldStateEnemy
        return $ ReversiStateMan ary
    where
        listtoary =[if x == 0 || x == 9 || y == 0 || y == 9 then FieldStateWall else FieldStateNone |x <- [0..9], y <- [0..9]]

-- Print Field.
printField :: Field -> IO ()
printField f = do
    -- Write divisions of X-axis 
    putChar ' '
    mapM_ (putStr . show) [0..9]
    putStrLn ""
    mapM_ (\i -> do
            putStr (show i)
            -- readArray f (j, i) >>= (putStr . show)
            -- equals ...
            --   auto cell = f[j][i];
            --   putStr(show(cell));
            mapM_ (\j -> readArray f (j, i) >>= (putStr . show)) [0..9]
            putStrLn ""
        ) [0..9]

-- list of 8-neibourhoods.
neibourhood8 :: [(Int, Int)]
neibourhood8 = [(1,0), (1,1), (0,1), (-1,0), (-1,-1), (0,-1), (-1, 1), (1, -1)]

-- check if x and y are in range.
-- Return: 
--     Nothing: x and y are in range.
--     Just   : x and y are not in range, String is an error message.
checkIsNotOutOfRange :: Int -> Int -> Maybe String
checkIsNotOutOfRange x y =
    if x >= 1 && y >= 1 && x <= 8 && y <= 8 then
        Nothing
    else
        Just "Please type 1 - 8."

-- Check if there is no stone in the cell which chosen.
-- Return:
--     Nothing: The cell is not filled.
--     Just   : The cell is filled, String is an error message.
checkHasNotPutOn :: Int -> Int -> Field -> IO (Maybe String)
checkHasNotPutOn x y field = do
    a <- readArray field (x, y)
    return $ case a of
        FieldStateNone -> Nothing
        _ -> Just "Stone was put."

-- Get how many stone the stone can reverse on the cell which chosen.
getReversibleStones :: Int -> Int -> FieldState -> Field -> IO [Int]
getReversibleStones x y user field = 
        mapM inner_ neibourhood8
    where
        -- for loop
        inner :: Int -> Int -> Int -> Int -> Int -> IO Int
        inner x y vx vy count =
            readArray field (x, y) >>= \a -> case a of
                FieldStateWall -> return 0
                FieldStateNone -> return 0
                _ ->
                    if a == user then 
                        return count
                    else
                        inner (x+vx) (y+vy) vx vy (count+1)
        inner_ (vx, vy) = inner (x+vx) (y+vy) vx vy 0

-- Put the stone, please call getReversible before call this.
-- Arguments:
--     reverseList: A return value of getReversibleStones.
-- Return:
--     Changed field.
putOn :: [Int] -> Int -> Int -> FieldState -> Field -> IO Field
putOn reverseList x y user field = do
        writeArray field (x, y) user
        mapM_ inner_ toRev
        return field
    where
        -- a function to check if it is puttable.
        checkIsPutable (a, (b, c)) = a > 0
        -- a list to reverse.
        toRev = ((map snd) . (filter checkIsPutable) . (zipWith (\a b -> (b, a)) neibourhood8)) reverseList
        -- loop function
        inner :: Int -> Int -> Int -> Int -> IO ()
        inner x y vx vy =
            readArray field (x, y) >>= \a -> case a of
                FieldStateWall -> return () 
                FieldStateNone -> return () 
                _ -> if a == user then 
                        return ()
                    else do
                        writeArray field (x, y) user
                        inner (x+vx) (y+vy) vx vy
        inner_ (vx, vy) = inner (x + vx) (y + vy) vx vy

-- Check if the user is playable(puttable).
checkPuttable :: FieldState -> Field -> IO Bool
checkPuttable user field =
    foldM (\p i ->
            if p then
                return True
            else
                foldM (\p j -> 
                        if p then
                            return True
                        else do
                            notPutOn <- checkHasNotPutOn i j field
                            case notPutOn of
                                Just ss -> return False
                                Nothing -> getReversibleStones i j user field >>= (return . (any (>0)))
                    ) False [1..8]
        ) False [1..8]

-- User inputting.
inputUser :: FieldState -> Field -> IO (Int, Int, [Int])
inputUser user field = do
        putStr "X: "
        hFlush stdout
        x <- readLn :: IO Int
        putStr "Y: "
        hFlush stdout
        y <- readLn :: IO Int
        case checkIsNotOutOfRange x y of
            Just s -> onError s
            Nothing -> do
                a <- checkHasNotPutOn x y field
                case a of
                    Just ss -> onError ss
                    Nothing -> do
                        res <- getReversibleStones x y user field
                        if (any (>0) res) == True then
                            return (x, y, res)
                        else
                            onError "You cannot put there."
    where
        onError msg = do
            putStrLn msg
            inputUser user field

-- Game Over.
-- Check which Man or Enemy has more points, and write console.
gameOver :: Field -> IO()
gameOver field = do
    (manPts, enemyPts) <- foldM (\d i -> do
            m <- foldM (\h j ->
                    let manPts = (fst h) :: Int
                        enemyPts = (snd h) :: Int
                    in do
                        b <- readArray field (j, i)
                        return $ case b of
                            FieldStateMan -> (manPts + 1, enemyPts)
                            FieldStateEnemy -> (manPts, enemyPts + 1)
                            _ -> (manPts, enemyPts)
                ) (0, 0) [0..9]
            return ((fst d) + (fst m), (snd d) + (snd m))
        ) (0, 0) [0..9]
    if manPts > enemyPts then
        putStrLn "YOU WIN!"
    else if manPts == enemyPts then
        putStrLn "DRAW"
    else
        putStrLn "YOU LOSE..."

-- main Loop
-- Arguments:
--      prevPassed: In previous turn, did the user against current user pass?
mainFunc :: Bool -> ReversiState -> IO ()
mainFunc prevPassed (ReversiStateMan field) = do
    printField field
    isPuttable <- checkPuttable FieldStateMan field
    if isPuttable then do
        putStrLn "Put Black stone."
        (x, y, bs) <- inputUser FieldStateMan field
        (putOn bs x y FieldStateMan field) >>= ((mainFunc False) . ReversiStateEnemy)
    else
        if prevPassed then
            gameOver field
        else
            ((mainFunc True) . ReversiStateEnemy) field


mainFunc prevPassed (ReversiStateEnemy field) = do
    printField field
    isPuttable <-  checkPuttable FieldStateEnemy field
    if isPuttable then do
        putStrLn "Put White stone."
        (x, y, bs) <- inputUser FieldStateEnemy field
        (putOn bs x y FieldStateEnemy field) >>= ((mainFunc False) . ReversiStateMan) 
    else
        if prevPassed then
            gameOver field
        else
            ((mainFunc True) . ReversiStateMan) field

someFunc :: IO ()
someFunc =
    initState >>= (mainFunc False)
