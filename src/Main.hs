import System.Random

data Interpreter = Interpreter 
                   { code :: Code
                   , pos :: Coords
                   , stack :: Stack 
                   , dir :: Direction 
                   , mode :: Mode 
                   , gen :: StdGen
                   , output :: String } deriving (Show)

type Code = [String]

type Coords = (Int, Int)

data Direction = L | R | U | D deriving (Eq,Show)

type Stack = [Int]

data Mode = None | Skip | Str deriving (Eq,Show)

main :: IO ()
main = do
       c <- getLine 
       g <- newStdGen
       putStrLn $ interpret g c 

--Setters and updaters for Interpreter

nextPos :: Interpreter -> Interpreter
nextPos (Interpreter c (x,y) s L m g o) = Interpreter c
                   ((x - 1) `mod` (length (head c)), y)
                   s L m g o
nextPos (Interpreter c (x,y) s R m g o) = Interpreter c
                   ((x + 1) `mod` (length (head c)), y)
                   s R m g o
nextPos (Interpreter c (x,y) s U m g o) = Interpreter c
                          (x, (y - 1) `mod` (length c))
                          s U m g o
nextPos (Interpreter c (x,y) s D m g o) = Interpreter c
                          (x, (y + 1) `mod` (length c))
                          s D m g o

push :: Int -> Interpreter -> Interpreter
push n (Interpreter c p s d m g o) = Interpreter c p (n:s) d m g o

dropS :: Interpreter -> Interpreter
dropS (Interpreter c p s d m g o) = Interpreter c p 
                   (if null s then [0] else tail s)
                   d m g o

setDir :: Direction -> Interpreter -> Interpreter
setDir d (Interpreter c p s _ m g o) = Interpreter c p s d m g o

setMode :: Mode -> Interpreter -> Interpreter
setMode m (Interpreter c p s d _ g o) = Interpreter c p s d m g o

addOutput :: Integer -> Interpreter -> Interpreter
addOutput n (Interpreter c p s d m g o) = Interpreter c p s d m g (o++(show n))

(!) :: Code -> Coords -> Char
(!) c (i, j) = c!!j!!i

-- Functions for interpretation

interpret :: StdGen -> String -> String
interpret g c = runProg $ Interpreter (lines c) (0, 0) [] R None g []

runProg :: Interpreter -> String
runProg i = interpCmd ((code i)!(0, 0)) i

interpCmd :: Char -> Interpreter -> String
interpCmd = undefined
