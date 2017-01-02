module Main(main) where

import Text.Parsec.Indent (runIndentParserT, IndentParserT, withBlock)
import Control.Monad.State.Lazy (evalState, State, modify, put, gets)
import Text.Parsec (many1, digit, anyChar, spaces, endOfLine, manyTill, char, optionMaybe)
import Control.Applicative ((<|>))
import Data.List (intercalate)
import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (stderr, hPrint)
import Data.Maybe (fromMaybe)

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

data Page = Page String (Maybe String) deriving (Show)
pageNumber = Page <$> many1 digit <* spaces <*> optionMaybe views
  where
  views = char '(' *> many1 digit <* char ')' <* spaces

parser :: IndentParserT String () Counter [(Page, Entry)]
parser = concat <$> many1 (withBlock comb pageNumber wordLine)
  where
  comb a = map ((,) a)

output items = "INSERT INTO vocab (list_number, page_number, description, num_reviews) VALUES " ++ values ++ ";"
  where
  toTuple (Page pageN views, Entry listN desc) = "(" ++ listN ++ ", " ++ pageN ++ ", \"" ++ desc ++ "\", " ++ fromMaybe "-1" views ++ ")"
  values = intercalate ", " $ map toTuple items

main = do
  parseResult <- (flip evalState undefined . runIndentParserT parser () "stdin") <$> getContents
  case parseResult of
    (Right items) -> putStrLn $ output items
    (Left err) -> hPrint stderr err >> exitFailure
