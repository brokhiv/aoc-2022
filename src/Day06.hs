module Day06 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day06 = ()

    puzzle :: Parser Day06
    puzzle = undefined
    
    solve1 :: Day06 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day06 -> Integer
    solve2 xs = undefined

    day06 = Day testCases puzzle solve1 solve2

    main = solveDay day06 "..\\input\\day06.txt"