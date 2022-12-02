module Day13 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day13 = ()

    puzzle :: Parser Day13
    puzzle = undefined
    
    solve1 :: Day13 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day13 -> Integer
    solve2 xs = undefined

    day13 = Day testCases puzzle solve1 solve2

    main = solveDay day13 "..\\input\\day13.txt"