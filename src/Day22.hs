module Day22 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day22 = ()

    puzzle :: Parser Day22
    puzzle = undefined
    
    solve1 :: Day22 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day22 -> Integer
    solve2 xs = undefined

    day22 = Day testCases puzzle solve1 solve2

    main = solveDay day22 "..\\input\\day22.txt"