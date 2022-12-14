module Day12 where
    import Data.List (elemIndex, findIndex)
    import Data.Maybe (fromJust)
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
        
    testCases = [(test, solve1, show 31)]

    data Location = S | N Int | E deriving (Show, Eq)

    type Day12 = [[Location]]

    puzzle :: Parser Day12
    puzzle = linesP row
        where   row = many1 location
                location = S <$ char 'S' <|> E <$ char 'E' <|> (\c -> N $ ord c - 97) <$> notChar '\n'
    
    elevation :: Location -> Int
    elevation S = 0
    elevation (N e) = e
    elevation E = 25

    isEdge :: Location -> Location -> Bool
    isEdge x y = (elevation y - elevation x) `elem` [-1..1]

    solve1 :: Day12 -> String
    solve1 xss = undefined --(\g -> length $ expectJust "A* failed" $ astar g (elemCoords S xss) (elemCoords E xss) heuristic)
                -- $ toGraph xss 
        -- where   elemCoords x xss = let r = fromJust $ findIndex (elem x) xss in (r, fromJust $ elemIndex x $ xss!!r)
                -- toGraph xss (i1, j1) = [(i2, j2) | i2 <- [0..length xss - 1], j2 <- [0..length (head xss) - 1], manhattan (i1, j1) (i2, j2) == 1, isEdge (xss!!i1!!j1) (xss!!i2!!j2)]
                -- heuristic (i, j) (ie, je) = max (elevation (xss!!ie!!je) - elevation (xss!!i!!j)) (manhattan (i, j) (ie, je))

    solve1' xss = (\g -> astar g (elemCoords S xss) (elemCoords E xss) heuristic)
                $ toGraph xss 
        where   elemCoords x xss = let r = fromJust $ findIndex (elem x) xss in (r, fromJust $ elemIndex x $ xss!!r)
                toGraph xss (i1, j1) = [(i2, j2) | i2 <- [max 0 (i1 - 1)..min (length xss - 1) (i1 + 1)], j2 <- [max 0 (j1 - 1)..min (length (head xss) - 1) (j1 + 1)], manhattan (i1, j1) (i2, j2) == 1, isEdge (xss!!i1!!j1) (xss!!i2!!j2)]
                heuristic (i, j) (ie, je) = max (elevation (xss!!ie!!je) - elevation (xss!!i!!j)) (manhattan (i, j) (ie, je))

    solve2 :: Day12 -> String
    solve2 xs = undefined

    day12 = Day testCases puzzle solve1 solve2

    main = solveDay day12 "..\\input\\day12.txt"