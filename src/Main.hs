import System.Random

data Interpreter = Interpreter 
                   { code :: Code
                   , pos :: Coords
                   , stack :: Stack 
                   , dir :: Direction 
                   , mode :: Mode 
                   , gen :: StdGen
                   , output :: String }

type Code = [String]

type Coords = (Int, Int)

data Direction = L | R | U | D deriving (Eq)

type Stack = [Int]

data Mode = None | Skip | Str deriving (Eq)

main :: IO ()
main = do
       c <- getLine 
       g <- newStdGen
       putStrLn $ interpret g c 

interpret :: StdGen -> String -> String
interpret g c = runProg $ Interpreter (lines c) (0, 0) [] R None g []

runProg :: Interpreter -> String
runProg = undefined
