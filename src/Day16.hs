module Day16 where
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
        
    testCases = [(test, solve1, show 1651)]

    data Valve = Valve { vid:: String, flow :: Int } deriving (Eq, Show)

    type Day16 = [(Valve, [String])]

    puzzle :: Parser Day16
    puzzle = linesP valve
        where valve = (\v f t -> (Valve v f, t)) <$> (string "Valve " *> countP 2 letter) <*> (string " has flow rate=" *> decimal) <*> (string "; tunnel" *> maybeP (char 's') *> string "lead" *> maybeP (char 's') *> string " to valves " *> sepBy1 (countP 2 letter) (string ", "))
    
    solve1 :: Day16 -> String
    solve1 xs = undefined
    
    solve2 :: Day16 -> String
    solve2 xs = undefined

    day16 = Day testCases puzzle solve1 solve2

    main = solveDay day16 "..\\input\\day16.txt"