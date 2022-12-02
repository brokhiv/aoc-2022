module Day20 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day20 = ()

    puzzle :: Parser Day20
    puzzle = undefined
    
    solve1 :: Day20 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day20 -> Integer
    solve2 xs = undefined

    day20 = Day testCases puzzle solve1 solve2

    main = solveDay day20 "..\\input\\day20.txt"