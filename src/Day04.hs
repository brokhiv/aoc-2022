module Day04 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day04 = ()

    puzzle :: Parser Day04
    puzzle = undefined
    
    solve1 :: Day04 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day04 -> Integer
    solve2 xs = undefined

    day04 = Day testCases puzzle solve1 solve2

    main = solveDay day04 "..\\input\\day04.txt"