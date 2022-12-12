module Parsing (
    Parser,
    (<?>),
    (<|>),
    T.anyChar,
    T.char,
    T.choice,
    T.count,
    T.decimal,
    T.digit,
    T.double,
    T.eitherP,
    T.endOfLine,
    T.hexadecimal,
    T.inClass,
    T.isEndOfLine,
    T.isHorizontalSpace,
    T.letter,
    T.many',
    T.many1,
    T.manyTill,
    T.match,
    T.notChar,
    T.notInClass,
    T.option,
    T.rational,
    T.satisfy,
    T.scientific,
    T.sepBy,
    T.sepBy1,
    T.signed,
    T.skip,
    T.skipSpace,
    T.skipWhile,
    T.space,
    T.take,
    T.takeTill,
    T.takeWhile,
    T.takeWhile1,
    ord,

    parse,
    parseTest,
    set,
    string,
    regex,
    linesP,
    hSpace,
    skipHSpace,
    maybeP,
    between,
    between2,
    parens,
    brackets,
    braces,
    angles,
    apos,
    quotes
) where
    import Control.Applicative ((<|>))
    import Data.Attoparsec.Text (Parser, (<?>))
    import qualified Data.Attoparsec.Text as T
    import Data.Char (ord)
    import Data.Text (pack, unpack)
    import Text.Regex (mkRegex, matchRegexAll)

    parse :: Parser a -> String -> a
    parse p text = case (T.parseOnly p $ pack text) of 
        Left err -> error err
        Right res -> res

    parseTest :: Show a => Parser a -> String -> IO ()
    parseTest p text = T.parseTest p $ pack text

    set :: [Char] -> Parser Char
    set xs = T.satisfy (\x -> x `elem` xs)

    string :: String -> Parser String
    string s = unpack <$> T.string (pack s)

    regex :: String -> Parser String
    regex r = undefined-- let (pre, m, post, subs) = matchRegexAll (mkRegex r) _ in if pre == "" && post == "" then m else _

    linesP :: Parser a -> Parser [a]
    linesP p = T.sepBy p T.endOfLine

    hSpace :: Parser Char
    hSpace = T.satisfy T.isHorizontalSpace <?> "hSpace"

    skipHSpace :: Parser ()
    skipHSpace = T.skipWhile T.isHorizontalSpace

    maybeP :: Parser a -> Parser (Maybe a)
    maybeP p = Just <$> p <|> pure Nothing

    between :: Parser o -> Parser c -> Parser a -> Parser a
    between o c a = o *> a <* c

    between2 :: Parser s -> Parser a -> Parser a
    between2 s = between s s

    parens :: Parser a -> Parser a
    parens = between (T.char '(') (T.char ')')

    brackets :: Parser a -> Parser a
    brackets = between (T.char '[') (T.char ']')

    braces :: Parser a -> Parser a
    braces = between (T.char '{') (T.char '}')

    angles :: Parser a -> Parser a
    angles = between (T.char '<') (T.char '>')

    apos :: Parser a -> Parser a
    apos = between2 (T.char '\'')

    quotes :: Parser a -> Parser a
    quotes = between2 (T.char '"')


