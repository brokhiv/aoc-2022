module Toolbox where
    import Data.Attoparsec.Text (Parser, satisfy)
    import Data.List (findIndex, nub)

    infixr 0 .>

    uncurry3 f (a, b, c) = f a b c

    set :: [Char] -> Parser Char
    set xs = satisfy (\x -> x `elem` xs)

    fun :: (Show a, Eq a) => [(a, b)] -> a -> b
    fun fs x = case lookup x fs of 
        Just y -> y
        Nothing -> error $ "Undefined for " ++ (show x)

    (?:) :: Maybe a -> a -> a
    Nothing ?: e = e
    (Just x) ?: _ = x

    match :: Eq a => [[a]] -> Maybe a
    match ([]:yss) = Nothing
    match ((x:xs):yss)
        | all (\ys -> x `elem` ys) yss = Just x
        | otherwise = match $ xs:yss

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = take n xs : (chunk n $ drop n xs)
    
    halve :: [a] -> [[a]]
    halve xs = chunk (length xs `div` 2) xs
    
    modifyAt :: Int -> (a -> a) -> [a] -> [a]
    modifyAt i f xs = (take i xs) ++ (f $ xs!!i) : (drop (i+1) xs)

    distinct :: Eq a => [a] -> Bool
    distinct xs = xs == nub xs

    window :: Int -> [a] -> [[a]]
    window _ [] = []
    window n (x:xs) 
        | length xs < n = []
        | otherwise = take n (x:xs) : (window n xs)

    findSublist :: Int -> ([a] -> Bool) -> [a] -> Maybe Int
    findSublist n f = window n .> findIndex f .> fmap (+n)

    (.>) :: (a -> b) -> (b -> c) -> (a -> c)
    (.>) = flip (.)
