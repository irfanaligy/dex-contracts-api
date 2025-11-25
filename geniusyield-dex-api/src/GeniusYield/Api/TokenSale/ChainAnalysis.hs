module GeniusYield.Api.TokenSale.ChainAnalysis
  ( RequestAnalysisParams (..)
  , Exposure (..)
  , ChainAnalysisResponse (..)
  , ApiToken
  , Risk (..)
  , registerAddress
  , requestAnalysis
  , registerAndAnalyze
  )
where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import GeniusYield.Imports (void)
import GeniusYield.Types
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (newTlsManager)

-- API token for ChainAnalysis
type ApiToken = BS.ByteString

-- request body parameters for requesting address https://docs.chainalysis.com/api/address-screening/#register-address
--
newtype RequestAnalysisParams = RequestAnalysisParams
  {reqAddress :: GYAddressBech32}
  deriving (Generic, Show)

instance FromJSON RequestAnalysisParams where
  parseJSON = withObject "RequestAnalysisParams" $ \r -> RequestAnalysisParams <$> r .: "address"

instance ToJSON RequestAnalysisParams where
  toJSON (RequestAnalysisParams addr) = object ["address" .= addr]

data Risk = Low | Medium | High | Severe deriving (Eq, FromJSON, Generic, Ord, Show, ToJSON)

data Cluster = Cluster
  { clName :: T.Text
  , clCategory :: T.Text
  }
  deriving Show

instance FromJSON Cluster where
  parseJSON = withObject "Cluster" $ \c -> Cluster <$> c .: "name" <*> c .: "category"

instance ToJSON Cluster where
  toJSON (Cluster name category) = object ["name" .= name, "category" .= category]

data Exposure = Exposure
  { exValue :: Integer
  , exCategory :: T.Text
  }
  deriving Show

instance FromJSON Exposure where
  parseJSON = withObject "Exposure" $ \c -> Exposure <$> c .: "value" <*> c .: "category"

instance ToJSON Exposure where
  toJSON (Exposure value category) = object ["value" .= value, "category" .= category]

data ChainAnalysisResponse = ChainAnalysisResponse
  { address :: GYAddressBech32
  , risk :: Risk
  , cluster :: Maybe Cluster
  , exposures :: Maybe [Exposure]
  }
  deriving (FromJSON, Generic, Show, ToJSON)

-- Registers address to chain analysis
-- https://docs.chainalysis.com/api/address-screening/#register-address
--

registerAddress :: ApiToken -> GYAddressBech32 -> IO RequestAnalysisParams
registerAddress token addr = do
  manager <- newTlsManager

  let
    url = "https://api.chainalysis.com/api/risk/v2/entities"
    body = Client.RequestBodyLBS $ encode $ RequestAnalysisParams addr
    initReq = Client.parseRequest_ url
    request = initReq {Client.method = "POST", Client.requestHeaders = [("Token", token)], Client.requestBody = body}

  Client.withResponse request manager $ \r -> do
    bdy <- Client.responseBody r
    case decode $ LBS.fromStrict bdy of
      Nothing -> fail $ T.unpack $ T.decodeUtf8 bdy
      Just d -> pure d

-- Request analysis for address
-- https://docs.chainalysis.com/api/address-screening/#retrieve-risk-assessment-for-a-registered-address
requestAnalysis :: ApiToken -> GYAddressBech32 -> IO ChainAnalysisResponse
requestAnalysis token address = do
  manager <- newTlsManager
  let
    addr = T.unpack $ addressToText $ addressFromBech32 address
    url = "https://api.chainalysis.com/api/risk/v2/entities/" <> addr
    initReq = Client.parseRequest_ url
    request = initReq {Client.requestHeaders = [("Token", token)]}

  Client.withResponse request manager $ \r -> do
    bdy <- Client.responseBody r
    case decode $ LBS.fromStrict bdy of
      Nothing -> fail $ T.unpack $ T.decodeUtf8 bdy
      Just d -> pure d

registerAndAnalyze :: ApiToken -> GYAddressBech32 -> IO ChainAnalysisResponse
registerAndAnalyze token addr = do
  void $ registerAddress token addr
  requestAnalysis token addr
