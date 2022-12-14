{-# LANGUAGE NamedFieldPuns #-}

module Day01 where
    import Parsing as P hiding (take)
    import Data.List (sortBy)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
        
    testCases = [(test, solve1, show 24000), (test, solve2, show 45000)]

    puzzle :: Parser [[Integer]]
    puzzle = sepBy1 parseList (endOfLine *> endOfLine)
        where parseList = sepBy1 decimal endOfLine
    
    solve1 :: [[Integer]] -> String
    solve1 = map sum .> maximum .> show
    
    solve2 :: [[Integer]] -> String
    solve2 = map sum .> sortBy (flip compare) .> take 3 .> sum .> show

    day01 = Day testCases puzzle solve1 solve2

    main = solveDay day01 "..\\input\\day01.txt"