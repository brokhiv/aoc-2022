module Day16 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day16 = ()

    puzzle :: Parser Day16
    puzzle = undefined
    
    solve1 :: Day16 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day16 -> Integer
    solve2 xs = undefined

    day16 = Day testCases puzzle solve1 solve2

    main = solveDay day16 "..\\input\\day16.txt"