module Day07 where
    import Control.Applicative ((<|>), many)
    import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, inClass, letter, many1, option, takeTill, satisfy, sepBy1, space)
    import Data.Text (unpack)

    import Common (solveDay, Day(Day))
    import Toolbox 

    test = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
        
    testCases = [(test, solve1, 95437), (test, solve2, 24933642)]

    type Day07 = [Command]

    data Command = CD String | LS [System] deriving (Show, Eq)

    data System = Dir [System] String | File Integer String deriving (Show, Eq)

    puzzle :: Parser Day07
    -- puzzle = ("$ " (cd | ls) "\n")+
    --     cd = "cd " /[./a-zA-Z]+/
    --     ls = "ls" file+
    --   file = "\n" ("dir " | /[0-9]+/ " ") filename
    -- fileName = /[a-zA-Z]*\.?[a-zA-Z]*/
    puzzle = sepBy1 (string "$ " *> (cd <|> ls)) endOfLine
        where   cd = CD <$> (string "cd " *> many (satisfy (inClass "./a-zA-Z")))
                ls = pure LS <$> (string "ls") <*> (many1 file) 
                file = endOfLine *> ((Dir [] <$> (string "dir " *> filename))
                        <|> (File <$> decimal <*> (space *> filename)))
                filename = (\n d e -> n++d++e) <$> many letter <*> (option [] (string ".")) <*> many letter

    size :: System -> Integer
    size (File s _) =  s
    size (Dir fs _) = (map size .> sum) fs

    name :: System -> String
    name (File _ s) = s
    name (Dir _ s) = s
    
    insert :: [System] -> [String] -> System -> System
    insert fs [] (Dir _ s) = Dir fs s
    insert fs (p:ps) (Dir sub s) = Dir (modifyWhere (\x -> name x == p) (insert fs ps) sub) s

    findDirs :: System -> (System -> Bool) -> [System]
    findDirs (File _ _) _ = []
    findDirs (Dir fs _) p = (filter (\f -> isDir f && p f) fs) ++ (concat $ map (flip findDirs p) fs)
        where   isDir (File _ _) = False
                isDir (Dir _ _) = True

    solve1 :: Day07 -> Integer
    solve1 = foldl cmd (Dir [] "/", []) .> fst .> (flip findDirs (\d -> size d <= 100000)) .> map size .> sum
        where   cmd (sys, _) (CD "/") = (sys, [])
                cmd (sys, ps) (CD "..") = (sys, init ps)
                cmd (sys, ps) (CD p) = (sys, ps ++ [p])
                cmd (sys, ps) (LS fs) = (insert fs ps sys, ps)
    
    solve2 :: Day07 -> Integer
    solve2 = foldl cmd (Dir [] "/", []) .> fst .> (\s -> ((flip findDirs (const True)) .> map size .> filter (>= 30000000 - (70000000 - size s))) s) .> minimum
        where   cmd (sys, _) (CD "/") = (sys, [])
                cmd (sys, ps) (CD "..") = (sys, init ps)
                cmd (sys, ps) (CD p) = (sys, ps ++ [p])
                cmd (sys, ps) (LS fs) = (insert fs ps sys, ps)

    day07 = Day testCases puzzle solve1 solve2

    main = solveDay day07 "..\\input\\day07.txt"