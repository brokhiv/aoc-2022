module Day08 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day08 = ()

    puzzle :: Parser Day08
    puzzle = undefined
    
    solve1 :: Day08 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day08 -> Integer
    solve2 xs = undefined

    day08 = Day testCases puzzle solve1 solve2

    main = solveDay day08 "..\\input\\day08.txt"