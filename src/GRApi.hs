{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GRApi (doShowShelf) where

import           Auth (signRequest, grAuthenticate, credz, defaultAuthHandler)
import           Control.Exception.Safe -- (IOException(..), catches, try, throw, Exception)
import           Control.Monad (guard)
import           Data.AppSettings (GetSetting(..), readSettings, FileLocation(AutoFromAppName), saveSettings, setSetting, Conf)
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Network.HTTP.Client (newManager, responseBody, Manager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Simple (Request, parseRequest, setRequestQueryString)
import           Settings
import           System.Environment (getEnv)
import           System.IO.Error (isDoesNotExistError)
import           Text.XML (parseText_, def)
import           Types
import           Web.Authenticate.OAuth (newCredential)
import           XML

-- | Auth Stuff: API Key and API Secret, get from system environment.
getKeysFromEnv :: IO AppCredentials
getKeysFromEnv = do
  grApiKey <- getEnv "GOODREADS_API_KEY"
  grApiSecret <- getEnv "GOODREADS_API_SECRET"
  return
    AppCredentials
    {applicationKey = pack grApiKey, applicationSecret = pack grApiSecret}

lookupConfig :: IO (Maybe (Conf, GetSetting))
lookupConfig =
  catchJust
    (guard . isDoesNotExistError)
    (Just <$> (readSettings (AutoFromAppName "goodreads")))
    (\_ -> return Nothing)

saveConfig :: GrConfig -> AppCredentials -> IO ()
saveConfig cfg _ -- conf? as 2nd arg
 = do
  let (tokenString, tokenSecretString) = credz (loginCredentials cfg)
  let defaultUID = fromMaybe 0 (defaultUserID cfg)
  let conf1 = setSetting Map.empty oAuthToken tokenString
  let conf2 = setSetting conf1 oAuthSecret tokenSecretString
  let conf3 = setSetting conf2 defaultUser defaultUID
  let conf4 = setSetting conf3 setApiKey "" -- appCredentials
  let conf5 = setSetting conf4 setApiSecret ""
  saveSettings defaultConfig (AutoFromAppName "goodreads") conf5

initGr :: Manager -> AuthRequest -> AuthHandler -> IO Gr
initGr man req authMethod = do
  x <- lookupConfig
  case x of
    Just (conf, GetSetting getSetting) -> do
      let secret = getSetting oAuthSecret
      case secret of
        "" -> do
          putStrLn "No OAuth token found" -- getnewsecrets?
          creds <- grAuthenticate man req authMethod
          let (tokenString, tokenSecretString) = credz creds
          putStrLn "Saved new OAauth token and secret to config file"
          let cfg =
                GrConfig
                { loginCredentials =
                    newCredential
                      (pack $ tokenString)
                      (pack $ tokenSecretString)
                , defaultUserID = Just (getSetting defaultUser)
                }
          let crd = (requestAppCredentials req)
          saveConfig cfg crd
          return $ Gr cfg man crd
        _ -> do
          let cfg =
                GrConfig
                { loginCredentials =
                    newCredential (pack $ getSetting oAuthToken) (pack $ secret)
                , defaultUserID = Just (getSetting defaultUser)
                }
          saveSettings defaultConfig (AutoFromAppName "goodreads") conf
          putStrLn "Loaded config file"
          return $ Gr cfg man (requestAppCredentials req)
    Nothing -> do
      putStrLn "No config file found."
      credentials <- grAuthenticate man req authMethod
      let (tokenString, tokenSecretString) = credz credentials
      putStrLn "Saved new OAauth token and secret to config file"
      let cfg =
            GrConfig
            { loginCredentials =
                newCredential (pack $ tokenString) (pack $ tokenSecretString)
            , defaultUserID = Nothing
            }
      let appCreds = (requestAppCredentials req)
      saveConfig cfg appCreds
      return $ Gr cfg man appCreds

-- | Begin Api Methods
restAPI
  :: MonadThrow m
  => Gr -> String -> [(ByteString, Maybe ByteString)] -> m Request
restAPI gr endpoint params
    -- Add API Key to params (if it is not in there FIXME?)
 = do
  let key = (BSU.toString (applicationKey (appCredentials gr)))
  let paramsWithKey =
        Map.toList $
        Map.insert (pack "key") (Just (pack key)) $ Map.fromList params
  req' <- parseRequest $ "https://www.goodreads.com/" ++ endpoint
  let request = setRequestQueryString paramsWithKey $ req'
  return request

doGr :: AppOptions -> IO Gr
doGr app = do
  keys <-
    try $
    case apiKey app of
      Just k -- Key was provided as argument
       ->
        return
          AppCredentials
          {applicationKey = pack k, applicationSecret = pack "NOT IMPLEMENTED"}
      Nothing -> do
        x <- lookupConfig
        case x of
          Just (_, GetSetting getSetting) -> do
            let api_Key = getSetting setApiKey
            let apiSecret = getSetting setApiSecret
            case (any null [api_Key, apiSecret]) of
              False ->
                return
                  AppCredentials
                  { applicationKey = pack api_Key
                  , applicationSecret = pack apiSecret
                  }
              True -> getKeysFromEnv -- not found in args, nor in config file.
          Nothing -> getKeysFromEnv
  case keys of
    Left (_ :: SomeException) ->
      error
        "Error Loading API Keys: Set GOODREADS_API_KEY, GOODREADS_API_SECRET"
    Right k -> do
      let authReq =
            AuthRequest
            { applicationName = "Gr"
            , expiration = Nothing
            , scope = [Read, Write]
            , requestAppCredentials = k
            }
      manager <- newManager tlsManagerSettings
      initGr manager authReq defaultAuthHandler

getBooksFromShelf
  :: MonadThrow m
  => Gr -> User -> String -> m Request
getBooksFromShelf conMan user shelf =
  restAPI conMan ("review/list/" ++ show (uid user) ++ ".xml") opts
  where
    opts =
      [ (pack "v", Just $ pack "2")
      , (pack "shelf", Just $ pack shelf)
      , (pack "per_page", Just $ pack "200")
      ] :: [(ByteString, Maybe ByteString)]

doShowShelf :: AppOptions -> ShelfName -> UserID -> IO (Map.Map T.Text Book)
doShowShelf opts shelf uID = do
  gr <- doGr opts
  let user_id =
        case uID of
          0 ->
            case defaultUserID (config gr) of
              Just u -> u
              Nothing -> uID -- try the default user
          _ -> uID
  req <-
    getBooksFromShelf
      gr
      User
      { uid = user_id -- fixme: this shouldn't ever be called with 0
      , name = Nothing
      }
      shelf
  resp <- signRequest gr req -- try
  let eBooks = respToBooks resp
  case eBooks of
    Right books -> do
      pure $ Map.fromList $ filter ((/= Just "") . review . snd) books
    Left _ -> do
      print req -- FIXME: Case debug
      fail "failed in parsing." -- FIXME: undefined -- some error in parsing See Throw, control.exceptions
  where
    respToBooks =
      parseGoodreadsFeed . parseText_ def . decodeUtf8 . responseBody

