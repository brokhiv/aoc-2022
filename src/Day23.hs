module Day23 where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day23 = ()

    puzzle :: Parser Day23
    puzzle = undefined
    
    solve1 :: Day23 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day23 -> Integer
    solve2 xs = undefined

    day23 = Day testCases puzzle solve1 solve2

    main = solveDay day23 "..\\input\\day23.txt"