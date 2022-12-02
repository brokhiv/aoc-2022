{-# LANGUAGE NamedFieldPuns #-}

module Day01 where
    import Data.Attoparsec.Text (Parser, endOfLine, sepBy1, decimal)
    import Data.List (sortBy)

    import Common (solveDay, Day(Day))

    test = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" -- Should be (24000, 45000)
        
    testCases = [(test, solve1, 24000), (test, solve2, 45000)]

    puzzle :: Parser [[Integer]]
    puzzle = sepBy1 parseList (endOfLine *> endOfLine)
        where parseList = sepBy1 decimal endOfLine
    
    solve1 :: [[Integer]] -> Integer
    solve1 xs = maximum $ map sum xs
    
    solve2 :: [[Integer]] -> Integer
    solve2 xs = sum $ take 3 $ sortBy (flip compare) $ map sum xs

    day01 = Day testCases puzzle solve1 solve2

    main = solveDay day01 "..\\input\\day01.txt"