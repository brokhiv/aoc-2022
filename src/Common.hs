{-# LANGUAGE AllowAmbiguousTypes #-}

module Common where
    import Data.Attoparsec.Text (Parser, parseOnly)
    import Data.Text (pack, Text)

    import Toolbox (uncurry3)

    type TestCase a b = (String, a -> b, b)

    data Day a b = Day {
        testCases :: [TestCase a b],
        puzzle :: Parser a,
        solve1 :: a -> b,
        solve2 :: a -> b
    }

    parse :: Parser a -> String -> a
    parse p text = case (parseOnly p $ pack text) of 
        Left err -> error err
        Right res -> res

    runTests :: Show b => Day a b -> [IO ()]
    runTests day = map (uncurry3 runTest) $ testCases day
        where runTest s f e = putStrLn $ "Expected: " ++ (show e) ++ ", got: " ++ ((show . f) $ parse (puzzle day) s)

    solveDay :: Show b => Day a b -> String -> IO ()
    solveDay day inputFile = do
        let tests = runTests day
        input <- readFile inputFile
        let parsedInput = parse (puzzle day) input

        putStrLn "Running tests..."
        sequence tests
        putStrLn "Tests done\n"
        putStrLn $ "Part 1: " ++ (show $ (solve1 day) parsedInput)
        putStrLn $ "Part 2: " ++ (show $ (solve2 day) parsedInput)