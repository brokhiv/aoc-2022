module Day24 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day24 = ()

    puzzle :: Parser Day24
    puzzle = undefined
    
    solve1 :: Day24 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day24 -> Integer
    solve2 xs = undefined

    day24 = Day testCases puzzle solve1 solve2

    main = solveDay day24 "..\\input\\day24.txt"