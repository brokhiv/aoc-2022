module Day10 where
    import Parsing as P hiding (takeWhile)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"
        
    testCases = [{-(test, solve1, [13140])-}]

    type Day10 = [Instr]

    data Instr = Noop Int | Addx Int deriving (Show, Eq)

    puzzle :: Parser Day10
    puzzle = linesP instr
        where instr = (const (Noop 1) <$> string "noop") <|> (Addx <$> (string "addx " *> signed decimal))
    
    isNoop :: Instr -> Bool
    isNoop (Noop _) = True
    isNoop _ = False

    getAdd :: Instr -> Int
    getAdd (Addx a) = a

    compact :: [Instr] -> [(Int, Int)]
    compact [] = []
    compact (Addx a : xs) = (2, a) : (compact xs)
    compact xs = (length $ takeWhile (isNoop) xs, 0) : (compact $ dropWhile (isNoop) xs)

    lookupFirst :: (Ord a, Eq a) => [(a, b)] -> b
    lookupFirst = undefined

    solve1 :: Day10 -> [(Int, Int)]
    solve1 = compact .> scanl exec (0, 0) -- .> trd3
        where exec (c, r) (ic, ia) = (c + ic, r + ia)
    
    solve2 :: Day10 -> [(Int, Int)]
    solve2 xs = undefined

    day10 = Day testCases puzzle solve1 solve2

    main = solveDay day10 "..\\input\\day10.txt"