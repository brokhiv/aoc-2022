module Day05 where
    import Data.Attoparsec.Text (Parser, char, choice, decimal, endOfLine, letter, sepBy1, skipWhile, space, string)
    import Data.Text (pack)
    import Data.List (transpose)
    import Data.Maybe (catMaybes)

    import Common (solveDay, Day(Day))

    test = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
        
    testCases = [(test, solve1, "CMZ"), (test, solve2, "MCD")]

    type Day05 = ([[Char]],[(Int, Int, Int)])

    puzzle :: Parser Day05
    puzzle = (,) <$> stacks <*> ((endOfLine *> skipWhile (flip elem "0123456789 ") *> endOfLine *> endOfLine) *> sepBy1 instr endOfLine)
            where   stacks = (map catMaybes . transpose) <$> sepBy1 (sepBy1 crate (char ' ')) endOfLine
                    crate = choice [Just <$> (char '[' *> letter <* char ']')
                            , const Nothing <$> string (pack "   ")]
                    instr = (,,) <$> (string (pack "move ") *> decimal) <*> (string (pack " from ") *> decimal) <*> (string (pack " to ") *> decimal)

    solve1 :: Day05 -> String
    solve1 (ss, is) = map (head) $ foldl (step) ss is
        where step ss (m, f, t) = (modifyAt (f-1) (drop m) . modifyAt (t-1) ((reverse $ take m $ ss!!(f-1)) ++)) ss
    
    solve2 :: Day05 -> String
    solve2 (ss, is) = map (head) $ foldl (step) ss is
        where step ss (m, f, t) = (modifyAt (f-1) (drop m) . modifyAt (t-1) ((take m $ ss!!(f-1)) ++)) ss

    day05 = Day testCases puzzle solve1 solve2

    main = solveDay day05 "..\\input\\day05.txt"