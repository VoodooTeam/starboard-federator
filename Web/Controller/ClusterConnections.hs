module Web.Controller.ClusterConnections where

import Data.ByteString.Base64 (decode, encode)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Web.Controller.Prelude hiding (decode, encode)
import Web.View.ClusterConnections.Index
import Web.View.ClusterConnections.New
import Web.View.ClusterConnections.Edit

tokenToDb :: (?frameworkConfig :: FrameworkConfig, Monad m) => Text -> m Text
tokenToDb tok = do
  let (AesKey key) = getAesKey
  pure $ decodeUtf8 $ encode $ aesCompute key $ encodeUtf8 tok

tokenFromDb :: (?frameworkConfig :: FrameworkConfig) => Text -> Text
tokenFromDb tok = do
  let (AesKey key) = getAesKey
  case decode $ encodeUtf8 tok  of
    Right b64' -> decodeUtf8 $ aesCompute key b64'
    Left _ -> ""

instance Controller ClusterConnectionsController where
    action ClusterConnectionsAction = do
        clusterConnections <- query @ClusterConnection |> fetch
        render IndexView { .. }

    action NewClusterConnectionAction = do
        let clusterConnection = newRecord
        render NewView { .. }

    action EditClusterConnectionAction { clusterConnectionId } = do
        let ?frameworkConfig = ?context |> getFrameworkConfig
        clusterConnection' <- fetch clusterConnectionId
        let clusterConnection = clusterConnection' {saToken = tokenFromDb $ get #saToken clusterConnection'}
        render EditView { .. }

    action UpdateClusterConnectionAction { clusterConnectionId } = do
        clusterConnection <- fetch clusterConnectionId
        clusterConnection
            |> buildClusterConnection
            |> validateClusterConnection
            >>= ifValid \case
                Left clusterConnection -> render EditView { .. }
                Right clusterConnection -> do
                    let ?frameworkConfig = ?context |> getFrameworkConfig
                    encryptedTok <- tokenToDb $ get #saToken clusterConnection
                    clusterConnection
                      |> set #saToken encryptedTok
                      |> updateRecord
                    setSuccessMessage "ClusterConnection updated"
                    redirectTo EditClusterConnectionAction { .. }

    action CreateClusterConnectionAction = do
        let clusterConnection = newRecord @ClusterConnection
        clusterConnection
            |> buildClusterConnection
            |> validateClusterConnection
            >>= ifValid \case
                Left clusterConnection -> render NewView { .. } 
                Right clusterConnection -> do
                    clusterConnection <- do
                      let ?frameworkConfig = ?context |> getFrameworkConfig
                      encryptedTok <- tokenToDb $ get #saToken clusterConnection
                      clusterConnection
                        |> set #saToken encryptedTok
                        |> createRecord
                    let job = newRecord @PollClusterJob
                    job
                      |> set #clusterId (Just (get #id clusterConnection))
                      |> create
                    clusterConnection <- clusterConnection |> createRecord
                    setSuccessMessage "ClusterConnection created"
                    redirectTo ClusterConnectionsAction

    action DeleteClusterConnectionAction { clusterConnectionId } = do
        clusterConnection <- fetch clusterConnectionId
        deleteRecord clusterConnection
        setSuccessMessage "ClusterConnection deleted"
        redirectTo ClusterConnectionsAction

buildClusterConnection clusterConnection = clusterConnection
    |> fill @["name","url","saToken"]


validateClusterConnection clusterConnection =
  clusterConnection
    |> validateField #name nonEmpty
    |> validateField #url isUrl
    |> validateIsUnique #name
    >>= validateIsUnique #url
