{-# LANGUAGE OverloadedStrings #-}
module XML where
import Types (Book(..))
import Text.XML.Lens
import Text.XML
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Applicative
import qualified Data.Text as T
import Data.Char

{- Functions for translating XML elements into objects. E.g. "book".
 -}
numReviews :: Document -> Int
numReviews doc = lengthOf ?? doc $ root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"

tities :: Document -> [Text]
tities doc = doc ^.. root . el "GoodreadsResponse" ./ el "reviews" ./ el "review" ./ el "book" ./ el "title" . text

parseBookInfo :: Document -> Maybe Text --Either String String
parseBookInfo doc =
    doc ^? root . el "GoodreadsResponse" ./ el "book"   ./ el "description" . text

parseBookSearch :: Document -> Either String [Book]
parseBookSearch doc =
    let bookElems = doc ^.. root . el "GoodreadsResponse" ./ el "search" ./ el "results" ./ el "work"
        books = catMaybes $ fmap parseFindBook bookElems

    in if null bookElems
      then Left $ "Unable to parse any items from " <> show doc
      else if (length bookElems) /= (length books)
           then Left $ "Unable to parse all items from " <> show bookElems
           else Right books

parseFindBook :: Element -> Maybe Book
parseFindBook e = Book
  <$> t "title"
  <*> Just (t "author")
  <*> Just (t "rating")
  <*> Just (t "review")
  where t n = e ^? el "work" ./ el "best_book" ./ el n . text

parseGoodreadsFeed :: Document -> Either String [(Text, Book)]
parseGoodreadsFeed doc =
  let bookElems = doc ^.. root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"
      books = catMaybes $ fmap parseBook bookElems

  in if null bookElems
      then Left $ "Unable to parse any items from " <> show doc
      else if (length bookElems) /= (length books)
           then Left $ "Unable to parse all items from " <> show bookElems
           else Right books

parseBook :: Element -> Maybe (Text, Book)
parseBook e = liftA2 (,) (t "id") $ Book
  <$> t "title"
  <*> Just (e ^? el "review" ./ el "book" ./ el "authors" ./ el "author" ./ el "name" . text)
  <*> Just (r "rating")
  <*> Just (fmap (T.dropWhileEnd isSpace . T.dropWhile isSpace) $ r "body")
  where
    t n = e ^? el "review" ./ el "book" ./ el n . text
    r n = e ^? el "review" ./ el n . text

