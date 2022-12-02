module Toolbox where
    import Data.Attoparsec.Text (Parser, satisfy)

    set :: [Char] -> Parser Char
    set xs = satisfy (\x -> x `elem` xs)

    fun :: (Show a, Eq a) => [(a, b)] -> a -> b
    fun fs x = case lookup x fs of 
        Just y -> y
        Nothing -> error $ "Undefined for " ++ (show x)
    
    