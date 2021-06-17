{-# LANGUAGE TypeApplications #-}

module Web.Controller.ClusterConnections where

import qualified Data.Text.Encoding as T (decodeUtf8)
import Web.Controller.Prelude
import Web.View.ClusterConnections.Edit
import Web.View.ClusterConnections.Index
import Web.View.ClusterConnections.New

instance Controller ClusterConnectionsController where
  --
  -- list all the cluster connections we have
  action ClusterConnectionsAction = do
    clusterConnections <- query @ClusterConnection |> fetch
    render IndexView {..}
  --
  -- create a brand new cluster connection
  action NewClusterConnectionAction = do
    let clusterConnection = newRecord
    render NewView {..}
  --
  -- fetch the selected cluster connection so we can edit it
  action EditClusterConnectionAction {clusterConnectionId} = do
    let ?frameworkConfig = ?context |> getFrameworkConfig
    cc <- fetch clusterConnectionId
    let saToken = fromMaybe "" $ tokenFromDb $ get #saToken cc
        clusterConnection = cc {saToken = T.decodeUtf8 saToken}
    render EditView {..}
  --
  -- update a cluster connection details
  action UpdateClusterConnectionAction {clusterConnectionId} = do
    clusterConnection <- fetch clusterConnectionId
    clusterConnection
      |> buildClusterConnection
      |> validateClusterConnection
      >>= ifValid \case
        Left clusterConnection -> render EditView {..}
        Right clusterConnection -> do
          let ?frameworkConfig = ?context |> getFrameworkConfig
          encryptedTok <- tokenToDb $ get #saToken clusterConnection
          clusterConnection
            |> set #saToken encryptedTok
            |> updateRecord
          setSuccessMessage "ClusterConnection updated"
          redirectTo EditClusterConnectionAction {..}
  --
  -- add a new cluster connection
  action CreateClusterConnectionAction = do
    let clusterConnection = newRecord @ClusterConnection
    clusterConnection
      |> buildClusterConnection
      |> validateClusterConnection
      >>= ifValid \case
        Left clusterConnection -> render NewView {..}
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
          --clusterConnection <- clusterConnection |> createRecord
          setSuccessMessage "ClusterConnection created"
          redirectTo ClusterConnectionsAction
  --
  -- delete a cluster connection
  action DeleteClusterConnectionAction {clusterConnectionId} = do
    clusterConnection <- fetch clusterConnectionId
    deleteRecord clusterConnection
    setSuccessMessage "ClusterConnection deleted"
    redirectTo ClusterConnectionsAction

buildClusterConnection clusterConnection =
  clusterConnection
    |> fill @["name", "url", "saToken"]

validateClusterConnection clusterConnection =
  clusterConnection
    |> validateField #name nonEmpty
    |> validateField #url isUrl
    |> validateIsUnique #name
    >>= validateIsUnique #url
