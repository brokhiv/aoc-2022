{-# LANGUAGE AllowAmbiguousTypes #-}

module Common where
    import Data.Attoparsec.Text (Parser, parseOnly)
    import Data.Text (pack, Text)

    uncurry3 f (a, b, c) = f a b c -- TODO import the right one

    type TestCase a = (String, a -> Integer, Integer)

    data Day a = Day {
        testCases :: [TestCase a],
        puzzle :: Parser a,
        solve1 :: a -> Integer,
        solve2 :: a -> Integer
    }

    parse :: Parser a -> String -> a
    parse p text = case (parseOnly p $ pack text) of 
        Left err -> error err
        Right res -> res

    runTests :: Day a -> [IO ()]
    runTests day = map (uncurry3 runTest) $ testCases day
        where runTest s f e = putStrLn $ "Expected: " ++ (show e) ++ ", got: " ++ ((show . f) $ parse (puzzle day) s)

    solveDay :: Day a -> String -> IO ()
    solveDay day inputFile = do
        let tests = runTests day
        input <- readFile inputFile
        let parsedInput = parse (puzzle day) input

        putStrLn "Running tests..."
        sequence tests
        putStrLn "Tests done\n"
        putStrLn $ "Part 1: " ++ (show $ (solve1 day) parsedInput)
        putStrLn $ "Part 2: " ++ (show $ (solve2 day) parsedInput)