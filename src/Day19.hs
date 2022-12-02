module Day19 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day19 = ()

    puzzle :: Parser Day19
    puzzle = undefined
    
    solve1 :: Day19 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day19 -> Integer
    solve2 xs = undefined

    day19 = Day testCases puzzle solve1 solve2

    main = solveDay day19 "..\\input\\day19.txt"