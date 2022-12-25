module Day21 where
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"
        
    testCases = [(test, solve1, show 152), (test, solve2, show 301)]

    data Op = Add | Sub | Mul | Div | Equ deriving (Eq, Show)

    data Job = Yell Int | Calc String Op String deriving (Eq, Show)

    type Day21 = [(String, Job)]

    puzzle :: Parser Day21
    puzzle = linesP monkey
        where   monkey = sepBy2 name job $ string ": "
                name = countP 4 letter
                job = Yell <$> decimal <|> (uncurry3 Calc) <$> sepBy3 name op name space
                op = const Add <$> char '+' <|> const Sub <$> char '-' <|> const Mul <$> char '*' <|> const Div <$> char '/'

    op :: Integral a => Op -> (a -> a -> a)
    op Add = (+)
    op Sub = (-)
    op Mul = (*)
    op Div = div
    op Equ = \x y -> if x == y then 0 else -1

    solve1 :: Day21 -> String
    solve1 = exec "root" .> show
        where exec x xs = case expectJust "Monkey not found" $ lookup x xs of
                            Yell n -> n
                            Calc a o b -> op o (exec a xs) (exec b xs)
    
    data Tree n l = Leaf l | Node (Tree n l) n (Tree n l) deriving (Eq, Show)

    reduceTree :: Tree Op (Maybe Int) -> Tree Op (Maybe Int)
    reduceTree (Leaf x) = Leaf x
    reduceTree (Node l o r) = case (reduceTree l, reduceTree r) of 
                (Leaf (Just x), Leaf (Just y)) -> Leaf (Just $ op o x y)
                (Leaf (Just x), Leaf Nothing) -> Node (Leaf (Just x)) o (Leaf Nothing)
                (Leaf Nothing, Leaf (Just y)) -> Node (Leaf Nothing) o (Leaf (Just y))
                (Leaf (Just x), Node l' o' r') -> Node (Leaf (Just x)) o (Node l' o' r')
                (Node l' o' r', Leaf (Just y)) -> Node (Node l' o' r') o (Leaf (Just y))

    invL, invR :: Integral a => Op -> (a -> a -> a)
    invL Add = (-)
    invL Sub = flip (-)
    invL Mul = div
    invL Div = flip div

    invR Add = (-)
    invR Sub = (+)
    invR Mul = div
    invR Div = (*)

    solve2 :: Day21 -> String
    solve2 = buildTree "root" .> reduceTree .> (\(Node l Equ r) -> equate l r) .> show
        where   buildTree "root" xs = case expectJust "Root not found" $ lookup "root" xs of
                            Calc a _ b -> Node (buildTree a xs) Equ (buildTree b xs)
                buildTree "humn" _ = Leaf Nothing
                buildTree x xs = case expectJust "Monkey not found" $ lookup x xs of
                            Yell n -> Leaf (Just n)
                            Calc a o b -> Node (buildTree a xs) o (buildTree b xs)
                equate fx (Leaf (Just y)) = case fx of
                            Leaf Nothing -> y
                            Node gx o (Leaf (Just c)) -> equate gx (Leaf $ Just $ invR o y c)
                            Node (Leaf (Just c)) o gx -> equate gx (Leaf $ Just $ invL o y c)
                equate (Leaf (Just y)) fx = equate fx (Leaf (Just y))

    day21 = Day testCases puzzle solve1 solve2

    main = solveDay day21 "..\\input\\day21.txt"