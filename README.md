# aoc-2022

This year, I do the Advent of Code in Haskell!

I run all my solutions from GHCi as of now, using the boilerplate code from ``Common.hs`` to first go through the tests and then print the solutions.

The basic pattern a solution conforms to is a set of test cases, a parser and two solve functions; to complete a day, the parser converts the input text into useful data, that then gets processed by both solve functions to get the (hopefully correct) answer.
