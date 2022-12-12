module Toolbox where
    import Data.List (findIndex, foldl', nub)
    import Data.Text (pack, unpack)

    infixr 0 .>

    uncurry3 f (a, b, c) = f a b c

    fst3 (x, _, _) = x

    snd3 (_, x, _) = x

    trd3 (_, _, x) = x

    fun :: (Show a, Eq a) => [(a, b)] -> a -> b
    fun fs x = case lookup x fs of 
        Just y -> y
        Nothing -> error $ "Undefined for " ++ (show x)

    (?:) :: Maybe a -> a -> a
    Nothing ?: e = e
    (Just x) ?: _ = x

    expectJust :: String -> Maybe a -> a
    expectJust err (Nothing) = error err
    expectJust _ (Just x) = x

    match :: Eq a => [[a]] -> Maybe a
    match ([]:yss) = Nothing
    match ((x:xs):yss)
        | all (\ys -> x `elem` ys) yss = Just x
        | otherwise = match $ xs:yss

    elem' :: (Eq a, Ord a) => a -> [a] -> Bool
    elem' x [] = False
    elem' x (x':xs)
        | x > x' = False
        | x == x' = True
        | otherwise = elem' x xs

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = take n xs : (chunk n $ drop n xs)
    
    halve :: [a] -> [[a]]
    halve xs = chunk (length xs `div` 2) xs
    
    modifyAt :: Int -> (a -> a) -> [a] -> [a]
    modifyAt i f xs = (take i xs) ++ (f $ xs!!i) : (drop (i+1) xs)

    modifyWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
    modifyWhere p f = map (\x -> if p x then f x else x)

    insertWith :: Ord b => (a -> b) -> a -> [a] -> [a]
    insertWith _ x [] = [x]
    insertWith f x (x':xs) = if f x <= f x' then x:x':xs else x' : (insertWith f x xs)

    distinct :: Eq a => [a] -> Bool
    distinct xs = xs == nub xs

    window :: Int -> [a] -> [[a]]
    window _ [] = []
    window n (x:xs) 
        | length xs < n = []
        | otherwise = take n (x:xs) : (window n xs)

    findSublist :: Int -> ([a] -> Bool) -> [a] -> Maybe Int
    findSublist n f = window n .> findIndex f .> fmap (+n)

    splitAround :: Int -> [a] -> ([a], a, [a])
    splitAround i xs = (take i xs, xs!!i, drop (i+1) xs)

    count :: (a -> Bool) -> [a] -> Int
    count p = filter p .> length

    countTrue :: [Bool] -> Int
    countTrue = count id

    -- | Like takeWhile, but includes the first element that violates the predicate.
    takeWhile' :: (a -> Bool) -> [a] -> [a]
    takeWhile' _ [] = []
    takeWhile' p (x:xs)
        | p x = x : (takeWhile' p xs)
        | otherwise = [x]

    manhattan :: Num a => (a, a) -> (a, a) -> a
    manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

    astar :: Eq a => [(a, a)] -> a -> a -> (a -> a -> Int) -> Maybe [a]
    astar g s e h = astar' [((s, h s e), 0)] [] [(s, 0)] []
        where   next g n = map snd $ filter ((== n) . fst) g
                findPath ps n = if n `elem` (map fst ps) then findPath ps (expectJust "Nothing in findPath" $ lookup n ps) ++ [n] else []
                astar' q vs cs ps
                    | null q = Nothing
                    | n == e = Just $ findPath ps n
                    | n `elem` vs = astar' q' vs cs ps
                    | otherwise = let expand = [(n', c', h n' e) | n' <- next g n, n' `notElem` vs, let c' = c + 1, (n' `notElem` (map fst cs)) || c' < expectJust "Nothing in expand" (lookup n' cs)] 
                                    in astar' 
                                        (foldl' (\xs (x, y, z) -> insertWith snd ((x, y), y + z) xs) q' expand)
                                        (nub $ n : vs)
                                        (cs ++ (map (\(x, y, _) -> (x, y)) expand))
                                        (ps ++ (map (\(x, _, _) -> (x, n))) expand)
                    where (((n, c), _), q') = (head q, tail q)
            

    (.>) :: (a -> b) -> (b -> c) -> (a -> c)
    (.>) = flip (.)
