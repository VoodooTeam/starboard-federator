module Web.View.ConfReports.ByCluster where

import Web.View.Prelude

newtype ByClusterView = ByClusterView {reports :: [ReportByCluster]}

data ReportByCluster = ReportByCluster {clusterId :: Id' "cluster_connections", name :: Text, score :: Int}

instance View ByClusterView where
  html ByClusterView {..} =
    [hsx|
        <h1>Scanned Clusters</h1>

        <a style="float:right" target="_blank" href={ClusterConnectionsAction}>Manage cluster connections</a>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Cluster</th>
                        <th>Risk Score</th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach reports renderReportByCluster}</tbody>
            </table>
        </div>
    |]

renderReportByCluster :: ReportByCluster -> Html
renderReportByCluster r =
  [hsx|
    <tr>
        <td>{get #name r}</td>
        <td>{get #score r }</td>
        <td><a href={ConfReportsInClusterAction (get #clusterId r)}>Show</a></td>
    </tr>
|]
