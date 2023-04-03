module Parse(parseExp, parseAll) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Syntax
import Types

assumeParser :: Parser Exp
assumeParser = Assume <$ string "assume" <* spaces <*> many1 letter
            <* char ':' <* spaces <*> pureTypeParser <* spaces
            <* string "->" <* spaces <*> pureTypeParser

pureTypeParser :: Parser PureType
pureTypeParser = try funcTypeParser <|> varTypeParser

effectsTypeParser :: Parser [String]
effectsTypeParser = (many1 letter) `sepBy` char ','

dirtyTypeParser :: Parser DirtyType
dirtyTypeParser = Dirt <$> pureTypeParser <* char '{' <*> effectsTypeParser <* char '}'

funcTypeParser :: Parser PureType
funcTypeParser = TArrow <$> varTypeParser <* string "=>" <*> dirtyTypeParser

varTypeParser :: Parser PureType
varTypeParser = TInt <$ string "int"

typeParser :: Parser Type
typeParser = try (PType <$> pureTypeParser) <|> DType <$> dirtyTypeParser

termParser :: Parser Exp
termParser = try assumeParser <|> try letinParser <|> try handlerParser
            <|> try performParser <|> try withhParser <|> try lamParser
            <|> try appParser <|> try plusParser <|> try numParser
            <|> varParser


lamParser :: Parser Exp
lamParser = Lam <$ char '(' <* string "fn" <* space <*> many1 letter
            <* spaces <* char ':' <* spaces <*> pureTypeParser <* spaces
            <* string "->" <* spaces <*> termParser <* char ')'

appParser :: Parser Exp
appParser = do
    char '('
    t1 <- termParser
    spaces
    t2 <- termParser
    char ')'
    return $ t1 :@: t2

plusParser :: Parser Exp
plusParser = do
    char '('
    t1 <- termParser
    spaces
    char '+'
    spaces
    t2 <- termParser
    char ')'
    return $ t1 :+: t2

letinParser :: Parser Exp
letinParser = Let <$ string "let" <* spaces <*> many1 letter
            <* spaces <* char '='
            <* spaces <*> termParser <* spaces <* string "in" <* spaces
            <*> termParser

handlerParser :: Parser Exp
handlerParser = Handler <$> (string "handler" *> spaces *> many1 letter)
            <*> ((,,) <$ spaces <* char '(' <* string "val" <* spaces <*> many1 letter
            <* char ':' <* spaces <*> pureTypeParser <* spaces
            <* string "->" <* spaces <*> termParser <* char ')')
            <*>((,,,,,) <$ spaces <* string "((" <*> many1 letter
            <* char ':' <* spaces <*> pureTypeParser <* char ','
            <* spaces <*> many1 letter <* char ':' <* spaces
            <*> pureTypeParser <* spaces <* string "->" <* spaces
            <*> dirtyTypeParser <* char ')' <* spaces <* string "->" <* spaces
            <*> termParser <* char ')')

performParser :: Parser Exp
performParser = Perform <$ string "perform" <* spaces <*> many1 letter
            <* spaces <*> termParser

withhParser :: Parser Exp
withhParser = WithH <$ string "with" <* spaces <*> termParser
            <* spaces <* string "handle" <* spaces <*> termParser

varParser :: Parser Exp
varParser = Var <$> many1 letter

numParser :: Parser Exp
numParser = (Int . read) <$> many1 digit

parseExp :: String -> Either ParseError Exp
parseExp = parse termParser ""

parseAll :: String -> [Either ParseError Exp]
parseAll = map parseExp . lines 
