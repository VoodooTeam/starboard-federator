{-# LANGUAGE TypeApplications #-}

module Web.Controller.ConfReports where

import qualified Data.Map as Map
import Web.Controller.Prelude
import Web.View.ConfReports.ByCluster
import Web.View.ConfReports.InCluster
import Web.View.ConfReports.Show

scoring :: ConfReport -> Int
scoring r = get #dangerCount r * 10 + get #warningCount r

reportsByCluster :: [ClusterConnection] -> [ConfReport] -> [ReportByCluster]
reportsByCluster conns reports =
  let connsById = Map.fromList $ fmap (\c -> (get #id c, get #name c)) conns
   in scoreByCluster
        |> Map.toList
        |> fmap (\(id, score) -> ReportByCluster id (fromMaybe "" (Map.lookup id connsById)) score)
        |> filter (\r -> get #name r /= "") -- remove the not found conns
        |> sortOn (\r -> (-1) * get #score r)
  where
    scoreByCluster =
      foldl
        ( \m r -> case Map.lookup (get #clusterId r) m of
            Nothing -> Map.insert (get #clusterId r) (scoring r) m
            Just _ -> Map.adjust (scoring r +) (get #clusterId r) m
        )
        (mempty :: Map (Id' "cluster_connections") Int)
        reports

instance Controller ConfReportsController where
  --
  -- global overview by cluster
  action ConfReportsByClusterAction = do
    confReports' <- query @ConfReport |> fetch
    conns' <- query @ClusterConnection |> fetch
    let reports = reportsByCluster conns' confReports'
    render ByClusterView {..}
  --
  -- overview for a specific cluster
  action ConfReportsInClusterAction {clusterId} = do
    c <- fetch clusterId
    rr <-
      query @ConfReport
        |> filterWhere (#clusterId, clusterId)
        |> fetch
    let reports = sortBy (\a b -> compare (scoring b) (scoring a)) rr
        clusterName = get #name c
    render InClusterView {..}
  --
  -- show report for a specific resource
  action ShowConfReportAction {confReportId} = do
    r <- fetch confReportId
    c <- fetch (get #clusterId r)
    case toReportChecks (get #report r) of
      Nothing -> renderNotFound
      Just cc -> do
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
