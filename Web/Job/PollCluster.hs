{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Web.Job.PollCluster where

import Control.Lens hiding (get, set, (|>))
import qualified Data.Aeson as JSON (decode, encode)
import Data.ByteString.Base64 (decode, encode)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.Connection as N (TLSSettings (..))
import qualified Network.HTTP.Client.TLS as N (mkManagerSettings)
import qualified Network.Wreq as N
import Web.Controller.Prelude hiding (decode)
import Web.Types

tokenFromDb :: (?context :: FrameworkConfig) => Text -> ByteString
tokenFromDb tok = do
  let ?frameworkConfig = ?context
  let (AesKey key) = getAesKey
      b64 = decode $ encodeUtf8 tok
  case b64 of
    Right b64' -> aesCompute key b64'
    Left _ -> "magic"

k8sAuthentication :: ByteString -> N.Options
k8sAuthentication token =
  N.defaults
    & N.auth ?~ N.oauth2Bearer token
    & N.header "Accept" .~ ["application/json"]
    -- only in dev mode (doesn't check cert)
    & N.manager .~ Left (N.mkManagerSettings (N.TLSSettingsSimple True False False) Nothing)

saveReport :: (?modelContext :: ModelContext) => Id' "cluster_connections" -> K8sConfReport -> IO ()
saveReport clusterId r = do
  newRecord @ConfReport
    |> set #clusterId clusterId
    |> set #namespace (get #resourceNamespace (get #labels (get #metadata r)))
    |> set #resourceKind (get #resourceKind (get #labels (get #metadata r)))
    |> set #resourceName (get #resourceName (get #labels (get #metadata r)))
    |> set #passCount (get #passCount (get #summary (get #report r)))
    |> set #dangerCount (get #dangerCount (get #summary (get #report r)))
    |> set #warningCount (get #warningCount (get #summary (get #report r)))
    |> set #report (fromReportChecks (get #report r)) -- (decodeUtf8 $ JSON.encode (get #report r))
    |> createRecord
  putStrLn "report inserted"

updateConfReports :: (?context :: FrameworkConfig, ?modelContext :: ModelContext) => ClusterConnection -> IO ()
updateConfReports c =
  do
    let url = get #url c
        token = tokenFromDb $ get #saToken c

    r <- N.getWith (k8sAuthentication token) (T.unpack $ url ++ "/apis/aquasecurity.github.io/v1alpha1/configauditreports")
    case r ^? N.responseBody of
      Nothing -> putStrLn "no body in response"
      Just body ->
        case JSON.decode body :: (Maybe K8sConfReports) of
          Nothing -> putStrLn "cannot decode response"
          Just rawConfReports -> do
            let save = saveReport (get #id c)
            mapM_ save (get #items rawConfReports)

instance Job PollClusterJob where
  perform PollClusterJob {..} =
    case clusterId of
      Nothing -> do
        clusters <- query @ClusterConnection |> fetch
        mapM_ updateConfReports clusters
      Just clusterId' -> do
        cluster <- fetch clusterId'
        updateConfReports cluster
