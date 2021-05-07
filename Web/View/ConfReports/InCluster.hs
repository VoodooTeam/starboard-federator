module Web.View.ConfReports.InCluster where

import Web.View.Prelude

data InClusterView = InClusterView {clusterName :: Text, reports :: [ConfReport]}

instance View InClusterView where
  html InClusterView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ConfReportsByClusterAction}>Clusters</a></li>
                <li class="breadcrumb-item active">Resources</li>
            </ol>
        </nav>
        <h1>Scanned Resources ({clusterName})</h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Resource</th>
                        <th>Danger</th>
                        <th>Warning</th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach reports renderReportInCluster}</tbody>
            </table>
        </div>
    |]

renderReportInCluster :: ConfReport -> Html
renderReportInCluster r =
  [hsx|
    <tr>
        <td>{get #namespace r}:{get #resourceName r}</td>
        <td>{get #dangerCount r}</td>
        <td>{get #warningCount r}</td>
        <td><a href={ShowConfReportAction (get #id r)}>Show</a></td>
    </tr>
|]
