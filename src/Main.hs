module Main where
    import Control.Monad (join)
    import Data.Time.Clock
    import Data.Time.Calendar
    import System.Environment (getArgs)

    import qualified Day01 (main)
    import qualified Day02 (main)
    import qualified Day03 (main)
    import qualified Day04 (main)
    import qualified Day05 (main)
    import qualified Day06 (main)
    import qualified Day07 (main)
    import qualified Day08 (main)
    import qualified Day09 (main)
    import qualified Day10 (main)
    import qualified Day11 (main)
    import qualified Day12 (main)
    import qualified Day13 (main)
    import qualified Day14 (main)
    import qualified Day15 (main)
    import qualified Day16 (main)
    import qualified Day17 (main)
    import qualified Day18 (main)
    import qualified Day19 (main)
    import qualified Day20 (main)
    import qualified Day21 (main)
    import qualified Day22 (main)
    import qualified Day23 (main)
    import qualified Day24 (main)
    import qualified Day25 (main)
    import Common

    days :: [IO ()]
    days =  [ Day01.main, Day02.main, Day03.main, Day04.main, Day05.main, Day06.main, Day07.main, Day08.main, Day09.main, Day10.main
            , Day11.main, Day12.main, Day13.main, Day14.main, Day15.main, Day16.main, Day17.main, Day18.main, Day19.main, Day20.main
            , Day21.main, Day22.main, Day23.main, Day24.main, Day25.main]
    
    getCurrentDay :: IO Int
    getCurrentDay = (\(_,_,d) -> d) <$> (getCurrentTime >>= return . toGregorian . utctDay)

    main :: IO ()
    main = do
        -- putStrLn "What do you want to run? ('' for today, 'day' for given day, 'all' for all days)"
        input <- getArgs
        if input == [] then join $ (\i -> days!!(i - 1)) <$> getCurrentDay 
            else if input == ["all"] then sequence_ days
            else days!!(((read $ headinput)::Int) - 1)