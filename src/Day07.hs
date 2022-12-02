module Day07 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day07 = ()

    puzzle :: Parser Day07
    puzzle = undefined
    
    solve1 :: Day07 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day07 -> Integer
    solve2 xs = undefined

    day07 = Day testCases puzzle solve1 solve2

    main = solveDay day07 "..\\input\\day07.txt"