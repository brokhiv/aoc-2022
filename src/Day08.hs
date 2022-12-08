module Day08 where
    import Data.Attoparsec.Text (Parser, digit, endOfLine, many1, sepBy1)
    import Data.Char (digitToInt)
    import Data.List (transpose)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "30373\n25512\n65332\n33549\n35390"
        
    testCases = [(test, solve1, 21), (test, solve2, 8)]

    type Day08 = [[Int]]

    puzzle :: Parser Day08
    puzzle = sepBy1 (many1 $ digitToInt <$> digit) endOfLine

    visible :: (Int, Int) -> [[Int]] -> Bool
    visible (r, c) xss = (check $ splitAround c (xss!!r)) || (check $ splitAround r ((transpose xss)!!c))
        where check (as, x, bs) = all (< x) as || all (< x) bs
    
    solve1 :: Day08 -> Int
    solve1 = (\xss -> [visible (r, c) xss | r <- [0..length xss - 1], c <- [0..length (head xss) - 1]]) .> countTrue
    
    scenicScore :: (Int, Int) -> [[Int]] -> Int
    scenicScore (r, c) xss = (calc $ splitAround c (xss!!r)) * (calc $ splitAround r ((transpose xss)!!c))
        where calc (as, x, bs) = (length $ takeWhile' (< x) $ reverse as) * (length $ takeWhile' (< x) bs)

    solve2 :: Day08 -> Int
    solve2 = (\xss -> [scenicScore (r, c) xss | r <- [0..length xss - 1], c <- [0..length (head xss) - 1]]) .> maximum

    day08 = Day testCases puzzle solve1 solve2

    main = solveDay day08 "..\\input\\day08.txt"