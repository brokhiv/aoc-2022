module Day05 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day05 = ()

    puzzle :: Parser Day05
    puzzle = undefined
    
    solve1 :: Day05 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day05 -> Integer
    solve2 xs = undefined

    day05 = Day testCases puzzle solve1 solve2

    main = solveDay day05 "..\\input\\day05.txt"