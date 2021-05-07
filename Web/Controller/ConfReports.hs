{-# LANGUAGE TypeApplications #-}

module Web.Controller.ConfReports where

import qualified Data.Aeson as JSON (decode)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T (encodeUtf8, encodeUtf8Builder)
import Web.Controller.Prelude
import Web.View.ConfReports.ByCluster
import Web.View.ConfReports.InCluster
import Web.View.ConfReports.Show

scoring :: ConfReport -> Int
scoring r = get #dangerCount r * 10 + get #warningCount r

reportsByCluster :: [ClusterConnection] -> [ConfReport] -> [ReportByCluster]
reportsByCluster conns reports =
  let connsById = Map.fromList $ fmap (\c -> (get #id c, get #name c)) conns
      scoreByCluster =
        foldl
          ( \m r -> case Map.lookup (get #clusterId r) m of
              Nothing -> Map.insert (get #clusterId r) (scoring r) m
              Just _ -> Map.adjust (scoring r +) (get #clusterId r) m
          )
          (mempty :: Map (Id' "cluster_connections") Int)
          reports
   in scoreByCluster
        |> Map.toList
        |> fmap (\(id, score) -> ReportByCluster id (fromMaybe "" (Map.lookup id connsById)) score)
        |> filter (\r -> get #name r /= "") -- remove the not found conns

instance Controller ConfReportsController where
  action ConfReportsByClusterAction = do
    confReports' <- query @ConfReport |> fetch
    conns' <- query @ClusterConnection |> fetch
    let reports = reportsByCluster conns' confReports'
    render ByClusterView {..}
  action ConfReportsInClusterAction {clusterId} = do
    c <- fetch clusterId
    reports' <-
      query @ConfReport
        |> filterWhere (#clusterId, clusterId)
        |> fetch
    let reports = sortBy (\a b -> compare (scoring b) (scoring a)) reports'
        clusterName = get #name c
    render InClusterView {..}
  action ShowConfReportAction {confReportId} = do
    r <-
      query @ConfReport
        |> filterWhere (#id, confReportId)
        |> fetchOne
    c <- fetch (get #clusterId r)
    case toReportChecks (get #report r) of
      Nothing -> renderNotFound
      Just cc ->
        do
          let confReport =
                ShowConfReport
                  { name = get #resourceName r,
                    namespace = get #namespace r,
                    clusterName = get #name c,
                    clusterId = get #id c,
                    checks = cc
                  }
          render ShowView {..}

buildConfReport confReport =
  confReport
    |> fill @["clusterId", "report"]
