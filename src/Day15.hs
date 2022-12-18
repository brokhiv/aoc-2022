module Day15 where
    import Parsing as P
    import Data.List (nub, union)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"
        
    testCases = [(test, solve1Test, show 26)]

    type Day15 = [((Int, Int), (Int, Int))]

    puzzle :: Parser Day15
    puzzle = linesP sensor
        where   sensor = string "Sensor at " *> ((,) <$> position) <*> (string ": closest beacon is at " *> position)
                position = string "x=" *> ((,) <$> signed decimal) <*> (string ", y=" *> signed decimal)
    
    solve1' :: Int -> Day15 -> String
    solve1' row = filter (\(s, b) -> abs (row - snd s) <= manhattan s b) .> (\sbs -> length (foldl1 union (map (\(s, b) -> let x = fst s; d = (manhattan s b) - abs (row - snd s) in [x - d .. x + d]) sbs)) - (count (snd .> (==row)) $ nub $ map snd sbs)) .> show

    solve1Test, solve1 :: Day15 -> String
    solve1Test = solve1' 10
    solve1 = solve1' 2000000
    
    solve2 :: Day15 -> String
    solve2 = undefined

    day15 = Day testCases puzzle solve1 solve2

    main = solveDay day15 "..\\input\\day15.txt"