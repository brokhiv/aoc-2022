module Day06 where
    import Data.Attoparsec.Text (Parser, takeWhile1)
    import Data.Maybe (fromJust)
    import Data.Text (unpack)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = ["mjqjpqmgbljsphdztnvjfqwrcgsmlb", "bvwbjplbgvbhsrlpgdmjqwftvncz", "nppdvjthqldpwncqszvftbrmjlhg", "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]
        
    testCases = (zipWith (\t e -> (t, solve1, e)) test [7, 5, 6, 10, 11]) ++ (zipWith (\t e -> (t, solve2, e)) test [19, 23, 23, 29, 26])

    type Day06 = String

    puzzle :: Parser Day06
    puzzle = unpack <$> takeWhile1 (const True)

    solve1 :: Day06 -> Int
    solve1 = findSublist 4 distinct .> fromJust
    
    solve2 :: Day06 -> Int
    solve2 = findSublist 14 distinct .> fromJust

    day06 = Day testCases puzzle solve1 solve2

    main = solveDay day06 "..\\input\\day06.txt"