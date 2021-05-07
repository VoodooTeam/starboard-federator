module Application.Helper.Controller where

import           "cryptonite" Crypto.Cipher.AES   (AES256)
import           "cryptonite" Crypto.Cipher.Types (ctrCombine, nullIV)
import qualified Data.TMap                        as TMap
import           IHP.ControllerPrelude
import qualified IHP.Log                          as Log
import           Web.Types                        
import qualified Data.Aeson as JSON (decode, encode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text.Encoding as T (encodeUtf8, encodeUtf8Builder, decodeUtf8)
import Data.ByteString.Lazy hiding (ByteString)

getAesKey :: (?frameworkConfig :: FrameworkConfig) => AesKey
getAesKey = ?frameworkConfig
        |> get #appConfig
        |> TMap.lookup @AesKey
        |> fromMaybe (error "Could not find AesKey in config")

aesCompute :: AES256 -> ByteString -> ByteString
aesCompute key  = ctrCombine key nullIV

toReportChecks :: Text -> Maybe ReportChecks
toReportChecks = JSON.decode . toLazyByteString . T.encodeUtf8Builder

fromReportChecks :: ReportChecks -> Text
fromReportChecks = T.decodeUtf8 . toStrict . JSON.encode 
