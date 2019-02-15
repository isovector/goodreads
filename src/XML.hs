{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module XML (parseGoodreadsFeed) where

import Types (Book(..))
import Text.XML.Lens
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Applicative
import qualified Data.Text as T
import Data.Char

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

