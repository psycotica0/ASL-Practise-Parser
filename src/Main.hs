module Main(main) where

import Text.Parsec.Indent (runIndentParserT, IndentParserT, withBlock)
import Control.Monad.State.Lazy (evalState, State, modify, put, gets)
import Text.Parsec (many1, digit, anyChar, spaces, endOfLine, manyTill, char)
import Control.Applicative ((<|>))
import Data.List (intercalate)
import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (stderr, hPrint)

type Counter = State Int

data Entry = Entry {listNumber :: String, description :: String} deriving (Show)

wordLine = Entry <$> numberOptions <* spaces <* char ')' <* spaces <*> manyTill anyChar endOfLine <* spaces
  where
  numberOptions = number <|> plus <|> same <|> assert
  number = do
    str <- many1 digit
    put $ read str
    return str
  plus = char '+' *> modify (1+) *> gets show
  same = char '&' *> gets show
  assert = do
    char '='
    target <- gets (1+)
    str <- many1 digit
    when (show target /= str) $ fail $ "Assertion failed, expected " ++ show target ++ " got " ++ str
    put target
    return str

pageNumber = many1 digit <* spaces

parser :: IndentParserT String () Counter [(String, Entry)]
parser = concat <$> many1 (withBlock comb pageNumber wordLine)
  where
  comb a = map ((,) a)

output items = "INSERT INTO vocab (list_number, page_number, description, num_reviews) VALUES " ++ values ++ ";"
  where
  toTuple (pageN, Entry listN desc) = "(" ++ listN ++ ", " ++ pageN ++ ", \"" ++ desc ++ "\", -1)"
  values = intercalate ", " $ map toTuple items

main = do
  parseResult <- (flip evalState undefined . runIndentParserT parser () "stdin") <$> getContents
  case parseResult of
    (Right items) -> putStrLn $ output items
    (Left err) -> hPrint stderr err >> exitFailure
