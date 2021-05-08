{-# LANGUAGE TypeApplications #-}

module Application.Helper.Controller where

import "cryptonite" Crypto.Cipher.AES (AES256)
import "cryptonite" Crypto.Cipher.Types (ctrCombine, nullIV)
import qualified Data.Aeson as JSON (decode, encode)
import qualified Data.ByteString.Base64 as B (decode, encode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.TMap as TMap
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8, encodeUtf8Builder)
import IHP.ControllerPrelude
import Web.Types

getAesKey :: (?frameworkConfig :: FrameworkConfig) => AesKey
getAesKey =
  ?frameworkConfig
    |> get #appConfig
    |> TMap.lookup @AesKey
    |> fromMaybe (error "Could not find AesKey in config")

aesCompute :: AES256 -> ByteString -> ByteString
aesCompute key = ctrCombine key nullIV

tokenFromDb :: (?frameworkConfig :: FrameworkConfig) => Text -> Maybe ByteString
tokenFromDb tok = do
  let (AesKey key) = getAesKey
  case B.decode $ T.encodeUtf8 tok of
    Right b64' -> Just $ aesCompute key b64'
    Left _ -> Nothing

tokenToDb :: (?frameworkConfig :: FrameworkConfig, Monad m) => Text -> m Text
tokenToDb tok = do
  let (AesKey key) = getAesKey
  pure $ T.decodeUtf8 $ B.encode $ aesCompute key $ T.encodeUtf8 tok

toReportChecks :: Text -> Maybe ReportChecks
toReportChecks = JSON.decode . toLazyByteString . T.encodeUtf8Builder

fromReportChecks :: ReportChecks -> Text
fromReportChecks = T.decodeUtf8 . toStrict . JSON.encode
