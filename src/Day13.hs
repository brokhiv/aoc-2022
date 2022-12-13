module Day13 where
    import Parsing as P
    import Data.List (elemIndex, intercalate, insert, sort)
    import Data.Maybe (fromJust)

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
        
    testCases = [(test, solve1, 13), (test, solve2, 140)]

    data Packet = Num Integer | List [Packet] deriving (Eq)

    type Day13 = [(Packet, Packet)]

    puzzle :: Parser Day13
    puzzle = sepBy1 pair (endOfLine *> endOfLine)
        where   pair = (,) <$> packet <*> (endOfLine *> packet)
                packet = fmap List $ brackets $ sepBy (packet <|> Num <$> decimal) (char ',')
    
    instance Ord Packet where
        compare (Num x) (Num y) = compare x y
        compare x@(Num _) ys@(List _) = compare (List [x]) ys
        compare xs@(List _) y@(Num _) = compare xs (List [y])
        compare (List xs) (List ys) = compare xs ys

    instance Show Packet where
        show (Num x) = show x
        show (List xs) = "[" ++ (intercalate "," $ map show xs) ++ "]"

    solve1 :: Day13 -> Int
    solve1 = indexed .> filter (snd .> (\(a, b) -> a < b)) .> map ((+1) . fst) .> sum
    
    solve2 :: Day13 -> Int
    solve2 = concatMap (\(a, b) -> [a,b]) .> sort .> insert (div1) .> insert (div2) .> (\xs -> (1 + (fromJust $ elemIndex div1 xs)) * (1 + (fromJust $ elemIndex div2 xs)))
        where   div1 = List [List [Num 2]]
                div2 = List [List [Num 6]]

    day13 = Day testCases puzzle solve1 solve2

    input = getParsedInput day13 "..\\input\\day13.txt"
    main = solveDay day13 "..\\input\\day13.txt"