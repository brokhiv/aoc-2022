module Day18 where
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"
        
    testCases = [(test, solve1, show 64), (test, solve2, show 58)]

    type Day18 = [(Int, Int, Int)]

    puzzle :: Parser Day18
    puzzle = linesP point
        where point = sepBy3 decimal decimal decimal (char ',')
    
    manhattan3 :: (Int, Int, Int) -> (Int, Int, Int) -> Int
    manhattan3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

    solve1 :: Day18 -> String
    solve1 xs = (map (\x -> 6 - neighbors x xs) .> sum .> show) xs
        where neighbors a = filter (\a' -> manhattan3 a a' == 1) .> length
    
    solve2 :: Day18 -> String
    solve2 = undefined

    day18 = Day testCases puzzle solve1 solve2

    main = solveDay day18 "..\\input\\day18.txt"