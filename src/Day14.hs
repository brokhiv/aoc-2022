module Day14 where
    import Parsing as P
    import Data.Function (fix)
    import Data.List (union)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
        
    testCases = [(test, solve1, show 24), (test, solve2, show 93)]

    type Day14 = [(Int, Int)]

    puzzle :: Parser Day14
    puzzle = fmap build $ linesP structure
        where   build = concatMap (paired .> map (\((x1, y1), (x2, y2)) -> [(x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]])) .> foldl1 union
                structure = sepBy1 coords $ string " -> "
                coords = (,) <$> decimal <*> (char ',' *> decimal)

    render :: [(Int, Int)] -> [String]
    render xys = [[if (x, y) `elem` xys then '#' else '.' | x <- [minimum (map fst xys) .. maximum (map fst xys)]] | y <- [minimum (map snd xys) .. maximum (map snd xys)]]

    solve1 :: Day14 -> String
    solve1 = (\xs -> (($*) spawnSand .> length) xs - length xs) .> show
        where   spawnSand xs = (let ((x, y), _) = fallSand ((500, 0), xs) in if y == maxBound then [] else [(x, y)]) ++ xs
                fallSand ((x, y), xs) = if y + 1 > maximum (map snd xs) then ((x, maxBound), xs)
                                        else if (x, y + 1) `notElem` xs then fallSand ((x, y + 1), xs)
                                        else if (x - 1, y + 1) `notElem` xs then fallSand ((x-1, y + 1), xs)
                                        else if (x + 1, y + 1) `notElem` xs then fallSand ((x + 1, y + 1), xs)
                                        else ((x, y), xs)
    
    solve2 :: Day14 -> String
    solve2 = (\xs -> length ((spawnSand $ maximum $ map snd xs) xs) - length xs) .> show
        where   spawnSand fl xs = if (500, 0) `elem` xs then xs else spawnSand fl $ (let ((x, y), _) = fallSand fl ((500, 0), xs) in [(x, y)]) ++ xs
                fallSand fl ((x, y), xs) = if y > fl then ((x, y), xs)
                                        else if (x, y + 1) `notElem` xs then fallSand fl ((x, y + 1), xs)
                                        else if (x - 1, y + 1) `notElem` xs then fallSand fl ((x-1, y + 1), xs)
                                        else if (x + 1, y + 1) `notElem` xs then fallSand fl ((x + 1, y + 1), xs)
                                        else ((x, y), xs)

    day14 = Day testCases puzzle solve1 solve2

    main = solveDay day14 "..\\input\\day14.txt"