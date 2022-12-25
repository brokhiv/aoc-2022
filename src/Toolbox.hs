{-# LANGUAGE ScopedTypeVariables #-}

module Toolbox where
    import Data.List ((\\), elemIndex, findIndex, foldl', insert, nub, sort)
    import Data.Text (pack, unpack)

    infixl 7 %
    infixr 0 .>, $*
    infixr 8 $^

    pair (a, b) = [a,b]
    triple (a, b, c) = [a, b, c]
    quad (a, b, c, d) = [a, b, c, d]
    quint (a, b, c, d, e) = [a, b, c, d, e]

    uncurry3 f (a, b, c) = f a b c
    uncurry4 f (a, b, c, d) = f a b c d
    uncurry5 f (a, b, c, d, e) = f a b c d e

    fst3 (x, _, _) = x
    snd3 (_, x, _) = x
    trd3 (_, _, x) = x

    fst4 (x, _, _, _) = x
    snd4 (_, x, _, _) = x
    trd4 (_, _, x, _) = x
    frt4 (_, _, _, x) = x

    fst5 (x, _, _, _, _) = x
    snd5 (_, x, _, _, _) = x
    trd5 (_, _, x, _, _) = x
    frt5 (_, _, _, x, _) = x
    fft5 (_, _, _, _, x) = x

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

    insertAll :: Ord a => [a] -> [a] -> [a]
    insertAll [] ys = ys
    insertAll (x:xs) ys = insertAll xs $ insert x ys 

    distinct :: Eq a => [a] -> Bool
    distinct xs = xs == nub xs

    paired :: [a] -> [(a, a)]
    paired xs = zip xs $ tail xs

    window :: Int -> [a] -> [[a]]
    window _ [] = []
    window n (x:xs) 
        | length xs < n = []
        | otherwise = take n (x:xs) : (window n xs)

    indexed :: [a] -> [(Int, a)]
    indexed = zip [0..]

    findSublist :: Int -> ([a] -> Bool) -> [a] -> Maybe Int
    findSublist n f = window n .> findIndex f .> fmap (+n)

    split :: Eq a => a -> [a] -> ([a], [a])
    split x xs = let (_:xs') = dropWhile (/= x) xs in (takeWhile (/= x) xs, xs')

    splitAround :: Int -> [a] -> ([a], a, [a])
    splitAround i xs = (take i xs, xs!!i, drop (i+1) xs)

    insertAt :: Int -> a -> [a] -> [a]
    insertAt i x xs = let (ls, rs) = (take i xs, drop i xs) in ls ++ x : rs

    deleteAt :: Int -> [a] -> [a]
    deleteAt i xs = let (ls, (_:rs)) = splitAt i xs in ls ++ rs

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

    maximal, minimal :: Ord a => Int -> [a] -> [a]
    maximal n xs = take n $ reverse $ sort xs
    minimal n xs = take n $ sort xs

    manhattan :: Num a => (a, a) -> (a, a) -> a
    manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

    astar :: (Eq a, Show a) => (a -> [a]) -> a -> a -> (a -> a -> Int) -> IO (Maybe [a])
    astar g s e h = astar' [((s, 0), h s e)] [] [(s, 0)] []
        where   findPath ps n = if n `elem` (map fst ps) then findPath ps (expectJust "Nothing in findPath" $ lookup n ps) ++ [n] else []
                astar' q vs cs ps
                    | null q = do {putStr "Visited: "; print vs; return Nothing}
                    | n == e = do { return $ Just $ findPath ps n }
                    | n `elem` vs = astar' q' vs cs ps
                    | otherwise = let expand = [(n', c', h n' e) | n' <- g n, n' `notElem` vs, let c' = c + 1, (n' `notElem` (map fst cs)) || c' < expectJust "Nothing in expand" (lookup n' cs)] 
                                    in do {putStr ("Expanding " ++ (show n) ++ ": "); print expand; astar' 
                                        (foldl' (\xs (x, y, z) -> insertWith snd ((x, y), y + z) xs) q' expand)
                                        (nub $ n : vs)
                                        (cs ++ (map (\(x, y, _) -> (x, y)) expand))
                                        (ps ++ (map (\(x, _, _) -> (x, n))) expand) }
                    where (((n, c), _), q') = (head q, tail q)

    (%) :: Integral a => a -> a -> a
    a % b = (a `mod` b + b) `mod` b

    (.>) :: (a -> b) -> (b -> c) -> (a -> c)
    (.>) = flip (.)

    ($^) :: (a -> a) -> Int -> (a -> a)
    f $^ 0 = id
    f $^ n = f . (f $^ (n-1))

    ($*) :: Eq a => (a -> a) -> a -> a
    f $* x = if f x == x then x else f $* (f x)
