module Day10 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day10 = ()

    puzzle :: Parser Day10
    puzzle = undefined
    
    solve1 :: Day10 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day10 -> Integer
    solve2 xs = undefined

    day10 = Day testCases puzzle solve1 solve2

    main = solveDay day10 "..\\input\\day10.txt"