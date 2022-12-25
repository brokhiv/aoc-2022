module Day25 where
    import Parsing as P

    import Common (solveDay, Day(Day))
    import Toolbox

    test = "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122"
        
    testCases = [(test, solve1, "2=-1=0")]

    type Day25 = [Int]

    fromSNAFU :: String -> Int
    fromSNAFU = reverse .> fromSNAFU'
        where   fromSNAFU' "" = 0
                fromSNAFU' (d:ds) = fromSNAFUdigit d + 5 * fromSNAFU' ds
                fromSNAFUdigit = fun [('=', -2), ('-', -1), ('0', 0), ('1', 1), ('2', 2)]

    toSNAFU :: Int -> String
    toSNAFU = (\x -> (x, "")) .> toSNAFU'
        where   toSNAFU' (0, ds) = ds
                toSNAFU' (x, ds) = let (x', d) = getDigit x in toSNAFU' (x', (toSNAFUdigit d) : ds)
                getDigit x = (\(d, m) -> (d, m-2)) $ (x + 2) `divMod` 5
                toSNAFUdigit = fun [(-2, '='), (-1, '-'), (0, '0'), (1, '1'), (2, '2')]

    puzzle :: Parser Day25
    puzzle = linesP snafu
        where snafu = fromSNAFU <$> (many1 $ set "=-012")
    
    solve1 :: Day25 -> String
    solve1 = sum .> toSNAFU
    
    solve2 :: Day25 -> String
    solve2 xs = undefined

    day25 = Day testCases puzzle solve1 solve2

    main = solveDay day25 "..\\input\\day25.txt"