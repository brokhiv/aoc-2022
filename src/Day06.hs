module Day{-DAY-} where
    import Data.Attoparsec.Text (Parser)

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day{-DAY-} = ()

    puzzle :: Parser Day{-DAY-}
    puzzle = undefined
    
    solve1 :: Day{-DAY-} -> Integer
    solve1 xs = undefined
    
    solve2 :: Day{-DAY-} -> Integer
    solve2 xs = undefined

    day{-DAY-} = Day testCases puzzle solve1 solve2

    main = solveDay day{-DAY-} "..\\input\\day{-DAY-}.txt"