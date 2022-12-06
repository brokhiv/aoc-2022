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

    solve1 :: Day03 -> Integer
    solve1 = map (halve .> match .> fmap priority .> (?: 0)) .> sum
    
    solve2 :: Day03 -> Integer
    solve2 = chunk 3 .> map (match .> fmap priority .> (?: 0)) .> sum

    day03 = Day testCases puzzle solve1 solve2

    main = solveDay day03 "..\\input\\day03.txt"