import System.Random
import System.IO.Unsafe
import Data.List.Split
import Data.List
import Data.Char
import Text.Printf

factorial :: Float -> Float
factorial n = (if n <= 0 then 1 else product [n,n-1.0..1.0])

binomialCdf :: Float -> Float -> Float -> Float
binomialCdf x n p = do
    let probabilities = [ (factorial x * p^(floor j) * (1-p)^(floor(x-j)))/
                          (factorial j * factorial (x-j))    
                         | j <- [n+1.0..x] ]
    sum probabilities

createSerialNumbers :: Int -> [[Int]]
createSerialNumbers n = unsafePerformIO $ do
    sg <- newStdGen
    let all_numbers = take (n*8) $ randomRs (0, 9) sg
    return $ chunksOf 8 all_numbers

raise :: Float -> Float -> (Bool, Float)
raise x n = do
    let baseline = (x - 2.0)/((2.0 * x) - 2.0)
    let odds = binomialCdf (8.0*(x-1.0)) (n-1.0) 0.1
    if odds > baseline 
        then (True, odds) 
        else (False, odds)

gameloop :: Float -> IO ()
gameloop x = do
    -- generate serial numbers for all players, display only yours
    let serial_numbers = createSerialNumbers (floor x)
    putStrLn "Your serial number: "
    print $ serial_numbers!!0
    
    putStrLn "Enter your move: "
    move <- getLine
    let n = fromIntegral $ digitToInt (move!!0)
    
    -- see how many you already have and calculate odds
    let m = fromIntegral $ length [l | l <- (serial_numbers!!0), l == (digitToInt(move!!2))]
    let r = raise x (n-m)
    
    -- advice
    if fst r == True
        then printf "Your odds are %.5f, you should raise...\n" $ snd r
        else printf "Your odds are %.5f, you should challenge...\n" $ snd r
    
    print serial_numbers
    
    -- continue?
    putStrLn "Continue? (y/n)"
    continue <- getLine
    if continue == "y"
        then gameloop x
        else putStrLn "Game over..."

main :: IO ()
main = do
    putStrLn "Liar's Poker"
    
    -- get the number of players
    putStrLn "Enter # of players: "
    num_players <- getLine
    let x = read num_players::Float
   
    gameloop x
    
    putStrLn "Thanks for playing!"