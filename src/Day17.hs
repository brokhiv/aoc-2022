module Day17 where
    import Parsing as P

    import Common (solveDay, Day(Day))

    test = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
        
    testCases = [(test, solve1, show 3068)]

    data Jet = Left | Right deriving (Eq, Show)

    type Day17 = [Jet]

    puzzle :: Parser Day17
    puzzle = many1 jet
        where jet = const Left <$> char '<' <|> const Right <$> char '>'

    rocks = cycle [ [[True, True, True, True]], 
                    [[False, True, False], [True, True, True], [False, True, False]],
                    [[False, False, True], [False, False, True], [True, True, True]],
                    [[True], [True], [True], [True]],
                    [[True, True], [True, True]]
                ]

    solve1 :: Day17 -> Integer
    solve1 = undefined
    
    solve2 :: Day17 -> Integer
    solve2 xs = undefined

    day17 = Day testCases puzzle solve1 solve2

    main = solveDay day17 "..\\input\\day17.txt"