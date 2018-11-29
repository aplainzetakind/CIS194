import System.Environment
import Data.Ord (comparing)
import Data.List (minimumBy)

newtype Peg = P Int deriving Show
data Move = M Peg Peg

instance Show Move where
    show (M (P n) (P m)) = show n ++ " -> " ++ show m

-- The first parameter is the number of pegs and the second is the number of
-- discs.
hanoi :: Int -> Int -> [Move]
hanoi p = hanoi' (map P [1..p])

-- hanoi' takes a list of pegs and a number of discs, and produces the move
-- sequence which will take all the discs from the fist peg in the list to the
-- second peg.
hanoi' :: [Peg] -> Int -> [Move]
hanoi' [] _              = error "Called zero pegs"
hanoi' [_] _             = error "Called one peg"
hanoi' [_, _] _          = error "Called two pegs"
hanoi' (p1:p2:_) 1       = [M p1 p2]
hanoi' [p1,p2,p3] n      = hanoi' [p1,p3,p2] (n - 1) ++
                           [M p1 p2] ++
                           hanoi' [p3,p2,p1] (n - 1)
hanoi' p@(p1:p2:p3:ps) d = hanoi' (p1:p3:p2:ps) k ++
                           hanoi' (p1:p2:ps) (d - k) ++
                           hanoi' (p3:p2:p1:ps) k
                             where k = minimumBy
                                          (comparing (solveLength l d)) [1..d-1]
                                   l = length p

solveLength :: Int -> Int -> Int -> Int
solveLength l d k = 2 * length (hanoi' (p l) k) +
                      length (hanoi' (p (l - 1)) (d - k))
                        where p m = map P [1..m]

main :: IO ()
main = do
        (x:y:_) <- getArgs
        let p = read x
            n = read y
        mapM_ print $ hanoi p n
