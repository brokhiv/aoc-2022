{-# LANGUAGE NamedFieldPuns #-}

module Day02 where
    import Data.Attoparsec.Text (Parser, endOfLine, satisfy, sepBy1, space)

    import Common (solveDay, Day(Day))
    import Toolbox 

    test = "A Y\nB X\nC Z"

    testCases = [(test, solve1, 15), ((test, solve2, 12))] 

    -- First implementation based on integers and offset modulo arithmetic --

    puzzle_old :: Parser [(Integer, Integer)]
    puzzle_old = sepBy1 parseRound endOfLine
        where   parseRound = (\s r -> (strategy s, strategy r)) <$> (set "ABC") <*> (space *> set "XYZ")
                strategy = fun [('A', 1), ('B', 2), ('C', 3), ('X', 1), ('Y', 2), ('Z', 3)]

    score1_old :: Integer -> Integer -> Integer
    score1_old a b
        | a == b = 3 + b -- tie
        | (a - b + 3) `mod` 3 == 1 = 0 + b -- loss
        | (a - b + 3) `mod` 3 == 2 = 6 + b -- win

    solve1_old :: [(Integer, Integer)] -> Integer
    solve1_old = sum . map (uncurry score1_old)

    score2_old :: Integer -> Integer -> Integer
    score2_old a 1 = ((a - 1 + 2) `mod` 3) + 1 + 0 -- pick worse and lose
    score2_old a 2 = a + 3 -- pick same and draw
    score2_old a 3 = ((a + 1 + 2) `mod` 3) + 1 + 6 -- pick better and win

    solve2_old :: [(Integer, Integer)] -> Integer
    solve2_old = sum . map (uncurry score2_old)

    -- Second implementation using pattern matching --

    data Strategy = Rock | Paper | Scissors deriving (Show, Eq)

    instance Ord Strategy where
        Rock <= Paper = True
        Paper <= Scissors = True
        Scissors <= Rock = True
        a <= b = a == b

    score :: Strategy -> Integer
    score Rock = 1
    score Paper = 2
    score Scissors = 3

    puzzle :: Parser [(Strategy, Char)]
    puzzle = sepBy1 parseRound endOfLine
        where   parseRound = (\s r -> (strategy s, r)) <$> set "ABC" <*> (space *> set "XYZ")
                strategy = fun [('A', Rock), ('B', Paper), ('C', Scissors)]

    solve1 :: [(Strategy, Char)] -> Integer
    solve1 = map (fmap (fun [('X', Rock), ('Y', Paper), ('Z', Scissors)]) .> uncurry eval) .> sum
        where eval s r = score r + case compare r s of
                LT -> 0
                EQ -> 3
                GT -> 6

    pick :: Strategy -> Ordering -> Strategy
    pick s EQ = s
    pick s GT = pick (pick s LT) LT
    pick Rock LT = Scissors
    pick Scissors LT = Paper
    pick Paper LT = Rock

    solve2 :: [(Strategy, Char)] -> Integer
    solve2 = map (fmap (fun [('X', LT), ('Y', EQ), ('Z', GT)]) .> uncurry eval) .> sum
        where eval s r = score (pick s r) + case r of
                LT -> 0
                EQ -> 3
                GT -> 6

    day02 = Day testCases puzzle solve1 solve2

    main = solveDay day02 "..\\input\\day02.txt"
