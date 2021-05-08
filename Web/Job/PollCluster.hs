{-# LANGUAGE TypeApplications #-}

module Web.Job.PollCluster where

import Config
import Control.Lens hiding (get, set, (|>))
import qualified Data.Aeson as JSON (decode, encode)
import qualified Data.Text as T (unpack)
import IHP.FrameworkConfig
import qualified Network.Connection as N (TLSSettings (..))
import qualified Network.HTTP.Client.TLS as N (mkManagerSettings)
import qualified Network.Wreq as N
import Web.Controller.Prelude hiding (decode)
import Web.Types

instance Job PollClusterJob where
  perform PollClusterJob {..} =
    case clusterId of
      Nothing -> do
        clusters <- query @ClusterConnection |> fetch
        mapM_ updateConfReports clusters
      Just clusterId' -> do
        cluster <- fetch clusterId'
        updateConfReports cluster

k8sAuthentication :: (?context :: FrameworkConfig) => ByteString -> N.Options
k8sAuthentication token =
  let opts =
        N.defaults
          & N.auth ?~ N.oauth2Bearer token
          & N.header "Accept" .~ ["application/json"]
   in if isDevelopment
        then opts & N.manager .~ Left (N.mkManagerSettings (N.TLSSettingsSimple True False False) Nothing) -- don't check cert in dev mode
        else opts

getSaToken :: (?context :: FrameworkConfig) => ClusterConnection -> Maybe ByteString
getSaToken c = do
  let ?frameworkConfig = ?context -- have to do this because tokenFromDb expects a ?frameworkConfig in context
  tokenFromDb $ get #saToken c

getConfReportsInCluster :: (?context :: FrameworkConfig) => ClusterConnection -> ByteString -> IO (Maybe K8sConfReports)
getConfReportsInCluster c t = do
  resp <- N.getWith (k8sAuthentication t) (T.unpack $ get #url c ++ "/apis/aquasecurity.github.io/v1alpha1/configauditreports")
  case resp ^? N.responseBody of
    Nothing -> noRespBody
    Just body ->
      case JSON.decode body :: (Maybe K8sConfReports) of
        Nothing -> cannotDecode
        reports -> pure reports
  where
    noRespBody = do
      putStrLn "no body in response"
      pure Nothing
    cannotDecode = do
      putStrLn "cannot decode response"
      pure Nothing

updateConfReports :: (?context :: FrameworkConfig, ?modelContext :: ModelContext) => ClusterConnection -> IO ()
updateConfReports c =
  case getSaToken c of
    Nothing -> noTokenFound
    Just t -> do
      mayConfReports <- getConfReportsInCluster c t
      case mayConfReports of
        Nothing -> pure () -- no reports in cluster
        Just confReports -> do
          let save = saveReport (get #id c) -- we just partially apply first not to have to do the job everytime
          mapM_ save (get #items confReports)
  where
    noTokenFound = do
      putStrLn "no token found"
      pure ()

saveReport :: (?modelContext :: ModelContext) => Id' "cluster_connections" -> K8sConfReport -> IO ()
saveReport clusterId r = do
  let labels = get #labels (get #metadata r)
      report = get #report r
      summary = get #summary report
  newRecord @ConfReport
    |> set #clusterId clusterId
    |> set #namespace (get #resourceNamespace labels)
    |> set #resourceKind (get #resourceKind labels)
    |> set #resourceName (get #resourceName labels)
    |> set #passCount (get #passCount summary)
    |> set #dangerCount (get #dangerCount summary)
    |> set #warningCount (get #warningCount summary)
    |> set #report (fromReportChecks report)
    |> createRecord
  putStrLn "report inserted"
