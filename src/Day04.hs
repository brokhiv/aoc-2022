module Day04 where
    import Data.Attoparsec.Text (Parser, sepBy1, char, decimal, endOfLine)
    import Data.List (intersect)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
        
    testCases = [(test, solve1, 2)]

    type Day04 = [((Integer, Integer),(Integer, Integer))]

    puzzle :: Parser Day04
    puzzle = sepBy1 pair endOfLine
        where   pair = (,) <$> assignment <*> (char ',' *> assignment)
                assignment = (,) <$> decimal <*> (char '-' *> decimal)
    
    solve1 :: Day04 -> Integer
    solve1 = filter (\(x, y) -> x `contains` y || y `contains` x) .> length .> toInteger
        where contains (xMin, xMax) (yMin, yMax) = (xMin <= yMin && xMax >= yMax)
    
    solve2 :: Day04 -> Integer
    solve2 = filter (uncurry overlap) .> length .> toInteger
        where overlap (xMin, xMax) (yMin, yMax) = not $ null $ [xMin..xMax] `intersect` [yMin..yMax]

    day04 = Day testCases puzzle solve1 solve2

    main = solveDay day04 "..\\input\\day04.txt"