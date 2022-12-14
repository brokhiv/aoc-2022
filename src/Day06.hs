module Day06 where
    import Parsing as P
    import Data.Maybe (fromJust)
    import Data.Text (unpack)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = ["mjqjpqmgbljsphdztnvjfqwrcgsmlb", "bvwbjplbgvbhsrlpgdmjqwftvncz", "nppdvjthqldpwncqszvftbrmjlhg", "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]
        
    testCases = (zipWith (\t e -> (t, solve1, show e)) test [7, 5, 6, 10, 11]) ++ (zipWith (\t e -> (t, solve2, show e)) test [19, 23, 23, 29, 26])

    type Day06 = String

    puzzle :: Parser Day06
    puzzle = unpack <$> takeWhile1 (const True)

    solve1 :: Day06 -> String
    solve1 = findSublist 4 distinct .> fromJust .> show
    
    solve2 :: Day06 -> String
    solve2 = findSublist 14 distinct .> fromJust .> show

    day06 = Day testCases puzzle solve1 solve2

    main = solveDay day06 "..\\input\\day06.txt"