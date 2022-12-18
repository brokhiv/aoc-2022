module Day11 where
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
        
    testCases = [(test, solve1, show 10605)]

    data Var = X | C Int deriving (Eq, Show)

    data Op = Times | Plus deriving (Eq, Show)

    data Monkey = Monkey {items:: [Int], operation :: (Var, Op, Var), next :: (Int, Int, Int)} deriving (Eq, Show)

    type Day11 = [Monkey]

    puzzle :: Parser Day11
    puzzle = sepBy1 monkey (countP 2 endOfLine)
        where   monkey = Monkey <$> (string "Monkey " *> digit *> char ':' *> skipSpace *> string "Starting items: " *> (sepBy decimal $ string ", ")) 
                                <*> (skipSpace *> string "Operation: " *> operation) <*> (next)
                operation = (,,) <$> (string "new = " *> (const X <$> string "old" <|> C <$> decimal)) 
                                <*> (between2 space $ const Times <$> char '*' <|> const Plus <$> char '+')
                                <*> (const X <$> string "old" <|> C <$> decimal)
                next = skipSpace *> string "Test: divisible by " *> ((,,) <$> decimal) <*> (skipSpace *> string "If true: throw to monkey " *> decimal) <*> (skipSpace *> string "If false: throw to monkey " *> decimal)
    
    exec :: Monkey -> Int -> Int
    exec (Monkey _ (a, o, b) _) x = (op o) (v a x) (v b x)
        where   op Times = (*)
                op Plus = (+)
                v X = id
                v (C c) = const c

    doNext :: Monkey -> Int -> Int
    doNext (Monkey _ _ (i, t, e)) x = if x `mod` i == 0 then t else e

    clearItem :: Monkey -> Monkey
    clearItem m = Monkey (tail $ items m) (operation m) (next m)

    addItem :: Monkey -> Int -> Monkey
    addItem m i = Monkey (items m ++ [i]) (operation m) (next m)

    solve1 :: Day11 -> String
    solve1 = (\xs -> (step $^ 1) (xs, replicate (length xs) 0)) {-.> snd .> maximal 2 .> product -}.> show
        where   step (ms, is) = (foldl turn (ms, replicate (length ms) 0) [0..length ms - 1])
                turn (ms, is) mi = (process (ms!!mi) ms, modifyAt mi (+ (length $ items $ ms!!mi)) is)
                process m@(Monkey [] _ _) ms = ms
                process m@(Monkey (i:is) op nx) ms = let i' = exec m i `div` 3 in (modifyWhere (==m) (const $ Monkey is op nx) . modifyAt (doNext m i') (flip addItem i')) ms
                    --foldl (\xs i -> let i' = exec m i `div` 3 in (modifyWhere () . modifyAt (doNext m i') (\x -> addItem x i')) ms) ms is
    
    solve2 :: Day11 -> String
    solve2 xs = undefined

    day11 = Day testCases puzzle solve1 solve2

    main = solveDay day11 "..\\input\\day11.txt"