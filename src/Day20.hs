module Day20 where
    import Parsing as P
    import Data.List (find)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "1\n2\n-3\n3\n-2\n0\n4"
        
    testCases = [(test, solve1, show 3)]

    type Day20 = [Int]

    puzzle :: Parser Day20
    puzzle = linesP $ signed decimal
    
    solve1 :: Day20 -> String
    solve1 = (\xs -> scanl step xs xs) .> show
        where   step ys x = let i = (fst $ expectJust "Not found" $ find (snd .> (==x)) $ indexed ys); r = (i + x) % length ys in (move i r x) ys
                move f t x ys = let (ls, _, rs) = splitAround f ys in if t < length ls then (insertAt t x ls) ++ rs else ls ++ (insertAt (t - 1 - length ls) x rs)
    
    solve2 :: Day20 -> String
    solve2 xs = undefined

    day20 = Day testCases puzzle solve1 solve2

    main = solveDay day20 "..\\input\\day20.txt"