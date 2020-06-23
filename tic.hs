import System.IO
import Data.Char
import Text.Printf
import Data.List

data Strength = Lose | Tie | Win
  deriving (Eq, Ord, Show)
type Field = Char
type Board = [[Field]]
type Move = (Pos, Char)
type Pos = (Int, Int)

emptyBoard :: Board
emptyBoard = ["   ", "   ", "   "]

showBoard :: Board -> String
showBoard b = printf "|0|1|2|\n-------\n|%v|%v|%v| 0\n-------\n|%v|%v|%v| 1\n-------\n|%v|%v|%v| 2\n-------\n" (valPos b (0,0)) (valPos b (1,0)) (valPos b (2,0)) (valPos b (0,1)) (valPos b (1,1)) (valPos b (2,1)) (valPos b (0,2)) (valPos b (1,2)) (valPos b (2,2))




main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "This is a game of tic tac toe.\nA proper input consists of 2 coordinates between 0 and 2, first the x, then the y"
  game emptyBoard 'x' otherwise

game :: Board -> Field -> Bool -> IO ()
game b c bot = do
  putStr (showBoard b)
  if (checkwin b) /= ' ' then
    putStrLn (checkwin b : " won the game!\nThanks for playing!")
  else do
    if (checktie b) then
      putStrLn "Tie!\nThanks for playing!"
    else do
      if bot && c == 'o' then do
        let new = safeNextBoard b (bestMove b c)
        game new (other c) bot
      else do
        inp <- getInput
        let p = (head inp, head (tail inp))
        let new = nextBoard b (p, c)
        case new of
          Just b' -> game b' (other c) bot
          Nothing -> do
            putStrLn "Error."
            game b c bot



other :: Field -> Field
other ' ' = 'x'
other 'x' = 'o'
other 'o' = 'x'

  
nextBoard :: Board -> Move -> Maybe Board
nextBoard b m 
  | (valPos b (fst m) == ' ') = Just (rep b m)
  | otherwise = Nothing
  where
    rep :: Board -> Move -> Board
    rep b m = [ [ posrep (x,y) b m | x <- [0..2]] | y <- [0..2]]
    posrep :: Pos -> Board -> Move -> Field
    posrep p b m
      | p == (fst m) = snd m
      | otherwise = valPos b p
safeNextBoard :: Board -> Move -> Board
safeNextBoard b m = aux next
  where
    next = nextBoard b m
    aux (Just a) = a
    aux Nothing = b

valPos :: Board -> Pos -> Field
valPos b p = (head (drop (fst p) (head (drop (snd p) b))))

getInput :: IO [Int]
getInput = do
  putStrLn "Specify coordinates: "
  aux (return [])
  where
    aux :: IO [Int] -> IO [Int]
    aux inp = do
    putStr "> "
    a <- inp
    infoif a
    line <- getLine
    let new = a ++ (inpFilter [reads n :: [(Int, String)] | n <- (words line)])
    if (length new) < 2 then
      (aux (return new))
    else do
      return (take 2 new)
    
infoif :: [Int] -> IO ()
infoif [] = return ()
infoif a = do
  putStr (("(x=" ++ (show (head a))) ++ ") ")

inpFilter :: [[(Int, String)]] -> [Int]
inpFilter a = [n | n <- readsFilter a, n < 3, n >= 0]
readsFilter :: [[(Int, String)]] -> [Int]
readsFilter a = [fst (head b) | b <- a, b /= []]

checkwin :: Board -> Char
checkwin b
  | hwin b /= ' ' = hwin b
  | vwin b /= ' ' = vwin b
  | dwin b /= ' ' = dwin b
  | otherwise = ' '

hwin :: Board -> Char
hwin [] = ' '
hwin (x:xs)
  | (haux x) == ' ' = hwin xs
  | otherwise = haux x
 
haux [] = ' '
haux (x:[]) = x
haux (x:xs)
  | x == haux xs = x
  | otherwise = ' '


vwin :: Board -> Char
vwin b = hwin $ vert b 

vert :: Board -> Board
vert b 
  | (head b) == [] = []
  | otherwise = [head row | row <- b] : vert (map tail b)

dwin :: Board -> Char
dwin [] = ' '
dwin b
  | daux1 b == ' ' = daux2 b 
  | otherwise = daux1 b

daux1 :: Board -> Char
daux1 [] = ' '
daux1 (x:[]) = head x
daux1 (x:a)
  | head x == daux1 xs = head x
  | otherwise = ' ' 
  where
    xs = (map tail a)


daux2 b = daux1 (reverse b)

checktie :: Board -> Bool
checktie b = not (or [or [a == ' ' | a <- arr] | arr <- b])


possiblePos :: Board -> [Pos]
possiblePos b = series [[(x,y) | x <- [0..2], valPos b (x,y) == ' '] | y <- [0..2]]
  where 
    series :: [[(Int,Int)]] -> [(Int,Int)]
    series [] = []
    series (x:xs) = x ++ (series xs)

boardStrength :: Board -> Field -> Strength
boardStrength b' c' = strAux b' c'
strAux b c
  | cur == 'x' = Win
  | cur == 'o' = Lose
  | checktie b = Tie
  | otherwise = winel c [strAux newb (other c)  | newb <- [safeNextBoard b (mov, c) | mov <- possiblePos b]] 
  where
    cur :: Char
    cur = checkwin b
winel :: Ord a => Field -> [a] -> a
winel 'x' = maximum
winel 'o' = minimum

safeElemIndex :: Eq a => a -> [a] -> Int
safeElemIndex x y = aux (elemIndex x y)
  where
    aux (Just a) = a
    aux Nothing = 0

bestMove :: Board -> Field -> Move
bestMove b c = (bestPos, c)
  where
    bestPos :: Pos
    bestPos = (possiblePos b) !! (safeElemIndex bestBoard boards)
    bestBoard :: Strength
    bestBoard = winel c boards
    boards :: [Strength]
    boards = [boardStrength (safeNextBoard b (m, c)) (other c) | m <- possiblePos b]


