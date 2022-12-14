{-# LANGUAGE AllowAmbiguousTypes #-}

module Common where
    import Data.Text (pack, Text)

    import Parsing
    import Toolbox (uncurry3)

    type TestCase a = (String, a -> String, String)

    data Day a = Day {
        testCases :: [TestCase a],
        puzzle :: Parser a,
        solve1 :: a -> String,
        solve2 :: a -> String
    }

    runTests :: Day a -> [IO ()]
    runTests day = map (uncurry3 runTest) $ testCases day
        where runTest s f e = do { putStrLn $ "Expected:"; putStrLn e; putStrLn $ "Got:\n" ++ (f $ parse (puzzle day) s) ++ "\n"}

    getParsedInput :: Day a -> String -> IO a
    getParsedInput day inputFile = do
        input <- readFile inputFile
        return $ parse (puzzle day) input

    solveDay :: Day a -> String -> IO ()
    solveDay day inputFile = do
        let tests = runTests day
        parsedInput <- getParsedInput day inputFile

        putStrLn "Running tests..."
        sequence tests
        putStrLn "Tests done\n"
        putStrLn $ "Part 1:\n" ++ ((solve1 day) parsedInput)
        putStrLn $ "Part 2:\n" ++ ((solve2 day) parsedInput)
        -- return parsedInput