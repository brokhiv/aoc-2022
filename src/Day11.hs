module Day11 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day11 = ()

    puzzle :: Parser Day11
    puzzle = undefined
    
    solve1 :: Day11 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day11 -> Integer
    solve2 xs = undefined

    day11 = Day testCases puzzle solve1 solve2

    main = solveDay day11 "..\\input\\day11.txt"