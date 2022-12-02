module Day03 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day03 = ()

    puzzle :: Parser Day03
    puzzle = undefined
    
    solve1 :: Day03 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day03 -> Integer
    solve2 xs = undefined

    day03 = Day testCases puzzle solve1 solve2

    main = solveDay day03 "..\\input\\day03.txt"