module Main(main) where

import Text.Parsec.Indent (runIndentParser, IndentParser, withBlock)
import Text.Parsec (many1, digit, anyChar, string, space, spaces, endOfLine, eof, manyTill, many, try, char)
import Control.Applicative ((<*>), (<|>), (<*), (*>))
import Data.Functor ((<$>), void)
import Data.List (intercalate)
import Control.Monad ((=<<))
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)

data Entry = Entry {listNumber :: String, description :: String} deriving (Show)

wordLine = Entry <$> many1 digit <* spaces <* char ')' <* spaces <*> manyTill anyChar endOfLine <* spaces

pageNumber = many1 digit <* spaces

parser :: IndentParser String () [(String, Entry)]
parser = concat <$> many1 (withBlock comb pageNumber wordLine)
  where
  comb a = map ((,) a)

output items = "INSERT INTO vocab (list_number, page_number, description, num_reviews) VALUES " ++ values ++ ";"
  where
  toTuple (pageN, Entry listN desc) = "(" ++ listN ++ ", " ++ pageN ++ ", \"" ++ desc ++ "\", -1)"
  values = intercalate ", " $ map toTuple items

main = do
  parseResult <- runIndentParser parser () "stdin" <$> getContents 
  case parseResult of
    (Right items) -> putStrLn $ output items
    (Left err) -> hPutStrLn stderr (show err) >> exitFailure
