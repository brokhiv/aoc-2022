module Day15 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day15 = ()

    puzzle :: Parser Day15
    puzzle = undefined
    
    solve1 :: Day15 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day15 -> Integer
    solve2 xs = undefined

    day15 = Day testCases puzzle solve1 solve2

    main = solveDay day15 "..\\input\\day15.txt"