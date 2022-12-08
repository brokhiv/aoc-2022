module Day12 where
    import Parsing as P

    import Common (solveDay, Day(Day))

    test = ""
        
    testCases = []

    type Day12 = ()

    puzzle :: Parser Day12
    puzzle = undefined
    
    solve1 :: Day12 -> Integer
    solve1 xs = undefined
    
    solve2 :: Day12 -> Integer
    solve2 xs = undefined

    day12 = Day testCases puzzle solve1 solve2

    main = solveDay day12 "..\\input\\day12.txt"