module Day11 where
    import Parsing as P

    import Common (solveDay, Day(Day))

    test = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
        
    testCases = []

    data Monkey = Monkey {items:: [Int], operation :: (Int -> Int), next :: (Int -> Int)}

    type Day11 = [Monkey]

    puzzle :: Parser Day11
    puzzle = sepBy1 monkey (endOfLine *> endOfLine)
        where   monkey = Monkey <$> (string "Monkey " *> digit *> char ':' *> skipSpace *> string "Starting items: " *> (sepBy decimal $ string ", ")) 
                                <*> (skipSpace *> string "Operation: " *> operation) <*> (next)
                operation = undefined
                next = undefined
    
    solve1 :: Day11 -> String
    solve1 xs = undefined
    
    solve2 :: Day11 -> String
    solve2 xs = undefined

    day11 = Day testCases puzzle solve1 solve2

    main = solveDay day11 "..\\input\\day11.txt"