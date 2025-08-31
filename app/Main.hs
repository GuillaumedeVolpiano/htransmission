{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           Control.Lens            ((.~), (^.))
import           Data.Aeson              (ToJSON, defaultOptions,
                                          genericToEncoding, toEncoding, toJSON)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Char8   (pack)
import qualified Data.ByteString.Lazy    as L (ByteString)
import           Data.CaseInsensitive    (CI, mk)
import           Data.Function           ((&))
import           Effectful               (Eff, runEff, (:>))
import           Effectful.Exception     (try)
import           Effectful.Reader.Static (Reader, ask, runReader)
import           Effectful.Wreq          (Response, Wreq, defaults, header,
                                          responseHeader, responseStatus,
                                          runWreq, statusCode)
import           Effectful.Wreq.Session  (Session, get, newSession, postWith)
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (HttpException (HttpExceptionRequest),
                                          HttpExceptionContent (StatusCodeException))
import           Options.Applicative     (Parser, auto, execParser, fullDesc,
                                          help, helper, info, long, metavar,
                                          option, progDesc, short, strOption,
                                          value, (<**>))

data Sesh = Sesh {
                  getSession  :: Session
                 , getFullUrl :: String
                 }

data Args = Args {
                 getHost   :: String
                 , getPort :: Int
                 , getUrl  :: String
                 }

instance Show Args where
  show (Args h p u) = h ++ ":" ++ show p ++ u

data Request = Request { method     :: String,
                          arguments :: Arguments
                        , tag       :: Maybe Int } deriving (Generic, Show)

data Arguments = TGet { ids :: Maybe [Int]
                 , fields   :: [String]
                 , format   :: Maybe String } |
                TGetAll { fields :: [String]
                        , format :: Maybe String }deriving (Generic, Show)

instance ToJSON Arguments where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions

args :: Parser Args
args =
  Args <$> strOption
             (long "host" <> short 'h' <> metavar "Host" <> value "http://localhost" <> help "link to the transmission host, like http://username:password@host")
      <*> option auto (long "port" <> short 'p' <> metavar "Port number" <> value 9091 <> help "port of the transmission host")
      <*> strOption (long "url" <> short 'u' <> metavar "URL" <> value "/transmission/rpc" <> help "url complement to the rpc interface")

sessionIdHeaderName :: CI ByteString
sessionIdHeaderName = mk . pack $ "X-Transmission-Session-Id"

getSessionId :: (Reader Sesh :> es, Wreq :> es) => Eff es ByteString
getSessionId = do
                (Sesh sesh url) <- ask
                failGet <- try (get sesh url)
                let sessionId (Right _) = error "Failed to open session with transmission"
                    sessionId (Left (HttpExceptionRequest _ (StatusCodeException s _)))
                                  | s ^. (responseStatus . statusCode) == 409 = s ^. responseHeader sessionIdHeaderName
                                  | otherwise = error "Failed to open session with transmission"
                    sessionId _ = error "Failed to open session with transmission"
                pure (sessionId failGet)

htransmission :: (Reader Sesh :> es, Wreq :> es) => Eff es (Response L.ByteString)
htransmission = do
                  (Sesh sesh url) <- ask
                  sessionId <- getSessionId
                  let opts = defaults & header sessionIdHeaderName .~ [sessionId]
                      examplePost = TGetAll ["id", "name", "totalSize"] Nothing
                      exampleRequest = toJSON $ Request "torrent-get" examplePost Nothing
                  postWith opts sesh url exampleRequest

main :: IO ()
main = do
  args' <- execParser . info (args <**> helper) $ (fullDesc <> progDesc "A command line to communicate with transmission-daemon")

  result <- runEff . runWreq $ do
                                  sesh <- newSession
                                  runReader (Sesh sesh (show args')) htransmission
  print result
