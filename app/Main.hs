{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified GRApi
import           Options.Applicative
import qualified Paths_gr as Meta (version)
import           Types

appOptions :: Parser AppOptions
appOptions = AppOptions
    <$> optional (strOption
        ( short 'k' <> long "with-key"
       <> metavar "APIKEY"
       <> help "Supply the API key as a command line argument."))

    <*> optional (option auto
        ( short 'l' <> long "limit"
       <> metavar "LIMIT"
       <> help "Limit the number of responses to LIMIT" ))

-- | Helper Function
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: Parser Options
parseOptions = Options <$> appOptions <* version <*> parseCommand

version :: Parser (a -> a)
version = infoOption (Data.Version.showVersion Meta.version)
  (  short 'v'
  <> long "version"
  <> help "Print version information" )


-- Commands
parseCommand :: Parser Command
parseCommand = subparser $
    command "show"          (parseShowShelf     `withInfo` "Show a shelf, e.GRApi. to-read")

parseShowShelf :: Parser Command
parseShowShelf = ShowShelf
    <$> argument str  (metavar "SHELFNAME")
    <*> argument auto (metavar "GOODREADS_USER_ID" <> value 0)  -- if default-user-id is defined in config, use it as default.

main :: IO ()
main =
    run =<<
    execParser (parseOptions `withInfo` "Interact with the Goodreads API. See --help for options.")

run :: Options -> IO ()
run (Options app cmd) = case cmd of
  ShowShelf shelfName uID -> do
    yo <- GRApi.doShowShelf app shelfName uID
    for_ yo $ \b -> do
      writeFile (mkFileName b) $ show b

mkFileName :: Book -> FilePath
mkFileName b = "site/books/" <> canonicalName b <> ".book"

canonicalName :: Book -> String
canonicalName = replace ' ' '-'
              . reverse . dropWhile (== ' ') . reverse
              . filter (liftM2 (||) isAlphaNum isSpace)
              . fmap toLower
              . liftM2 spaceConcat (maybe "" T.unpack . author)
                                   (T.unpack . title)
  where spaceConcat a b = a ++ " " ++ b
        replace a b = map $ \c -> if (c == a) then b else c

