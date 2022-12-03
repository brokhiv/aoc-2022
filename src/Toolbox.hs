module Toolbox where
    import Data.Attoparsec.Text (Parser, satisfy)

    set :: [Char] -> Parser Char
    set xs = satisfy (\x -> x `elem` xs)

    fun :: (Show a, Eq a) => [(a, b)] -> a -> b
    fun fs x = case lookup x fs of 
        Just y -> y
        Nothing -> error $ "Undefined for " ++ (show x)

    (?:) :: Maybe a -> a -> a
    Nothing ?: e = e
    (Just x) ?: _ = x

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = take n xs : (chunk n $ drop n xs)
