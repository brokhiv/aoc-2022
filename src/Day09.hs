module Day09 where
    import Data.List (nub)
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = ["R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2", "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"]
        
    testCases = [(test!!0, solve1, 13), (test!!0, solve2, 1), (test!!1, solve2, 36)]

    type Day09 = [(Direction, Int)]

    data Direction = U | R | L | D deriving (Show, Eq)

    puzzle :: Parser Day09
    puzzle = linesP step
        where step = (,) <$> (const U <$> char 'U' <|> const R <$> char 'R' <|> const L <$> char 'L' <|> const D <$> char 'D') <*> (skipSpace *> decimal)
    
    move :: Direction -> (Int, Int) -> (Int, Int)
    move U (x, y) = (x, y + 1)
    move R (x, y) = (x + 1, y)
    move L (x, y) = (x - 1, y)
    move D (x, y) = (x, y - 1)

    -- |Not proud of this function, but it does the job. Maybe some maths could get follow' working tho
    follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
    follow h@(hx, hy) t@(tx, ty)
        | (abs (hx - tx) <= 1) && (abs (hy - ty) <= 1) = t
        | (abs (hx - tx) <= 1) && hy > ty + 1 = (hx, hy - 1)
        | (abs (hx - tx) <= 1) && hy < ty - 1 = (hx, hy + 1)
        | hx > tx + 1 && (abs (hy - ty) <= 1) = (hx - 1, hy)
        | hx < tx - 1 && (abs (hy - ty) <= 1) = (hx + 1, hy)
        | hx > tx + 1 && hy > ty + 1 = (hx - 1, hy - 1)
        | hx > tx + 1 && hy < ty - 1 = (hx - 1, hy + 1)
        | hx < tx - 1 && hy > ty + 1 = (hx + 1, hy - 1)
        | hx < tx - 1 && hy < ty - 1 = (hx + 1, hy + 1)
        | otherwise = error ("follow broke on " ++ (show h) ++ " " ++ (show t))

    follow' :: (Int, Int) -> (Int, Int) -> (Int, Int)
    follow' h@(hx, hy) t@(tx, ty) = (go hx tx, go hy ty)
        where go a b
                | abs (a - b) <= 1 = b
                | a > b + 1 = b + 1
                | a < b - 1 = b - 1
                | otherwise = error ("follow broke on " ++ (show h) ++ " " ++ (show t))

    solve1 :: Day09 -> Int
    solve1 = foldl step ((0,0), [(0, 0)]) .> snd .> nub .> length
        -- step :: ((Int, Int), [(Int, Int)]) -> (Direction, Int) -> ((Int, Int), [(Int, Int)])
        where   step (h, (t:ts)) (d, 0) = (h, (t:ts))
                step (h, (t:ts)) (d, n) = let h' = move d h in step (h', follow h' t : (t:ts)) (d, n - 1)
    
    solve2 :: Day09 -> Int
    solve2 = foldl step (replicate 9 (0, 0), [(0,0)]) .> snd .> nub .> length
        -- step :: ([(Int, Int)], [(Int, Int)]) -> (Direction, Int) -> ([(Int, Int)], [(Int, Int)])
        where   step (h:hs, t:ts) (d, 0) = (h:hs, t:ts)
                step (h:hs, t:ts) (d, n) = let h' = move d h; hs' = scanl1 follow (h':hs) in step (hs', follow (last hs') t : (t:ts)) (d, n - 1)

    day09 = Day testCases puzzle solve1 solve2

    main = solveDay day09 "..\\input\\day09.txt"