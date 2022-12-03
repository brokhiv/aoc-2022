module Day03 where
    import Data.Attoparsec.Text (Parser, endOfLine, letter, many1, sepBy1)
    import Data.Char (ord, isLower)
    import Data.Text (unpack)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
        
    testCases = [(test, solve1, 157), (test, solve2, 70)]

    type Day03 = [[Char]]

    puzzle :: Parser Day03
    puzzle = sepBy1 (many1 letter) endOfLine
    
    priority :: Char -> Integer
    priority c
        | isLower c = toInteger (ord c) - 96
        | otherwise = toInteger (ord c) - 38

    halve :: [a] -> [[a]]
    halve xs = chunk (length xs `div` 2) xs

    match2 :: Eq a => [a] -> [a] -> Maybe a
    match2 [] ys = Nothing
    match2 (x:xs) ys
        | x `elem` ys = Just x
        | otherwise = match2 xs ys

    solve1 :: Day03 -> Integer
    solve1 = map (halve .> match .> fmap priority .> (?: 0)) .> sum

    match :: Eq a => [[a]] -> Maybe a
    match ([]:yss) = Nothing
    match ((x:xs):yss)
        | all (\ys -> x `elem` ys) yss = Just x
        | otherwise = match $ xs:yss
    
    solve2 :: Day03 -> Integer
    solve2 = chunk 3 .> map (match .> fmap priority .> (?: 0)) .> sum

    day03 = Day testCases puzzle solve1 solve2

    main = solveDay day03 "..\\input\\day03.txt"