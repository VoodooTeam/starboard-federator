module Web.View.ConfReports.Show where

import Data.Map (toList)
import Web.View.Prelude

newtype ShowView = ShowView {confReport :: ShowConfReport}

data ShowConfReport = ShowConfReport
  { checks :: ReportChecks,
    namespace :: Text,
    clusterName :: Text,
    name :: Text,
    clusterId :: Id' "cluster_connections"
  }

filterPassing :: [ConfigCheck] -> [ConfigCheck]
filterPassing = filter (not . get #success)

instance View ShowView where
  html ShowView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ConfReportsByClusterAction}>Clusters</a></li>
                <li class="breadcrumb-item"><a href={ConfReportsInClusterAction (get #clusterId confReport)}>Resources</a></li>
                <li class="breadcrumb-item active">Report</li>
            </ol>
        </nav>
        <h1>{get #clusterName confReport}/{get #namespace confReport}:{get #name confReport}</h1>
        <div style="float:right">
          explanations : 
            <a href="https://polaris.docs.fairwinds.com/checks/security/" target="_blank">security</a>,
            <a href="https://polaris.docs.fairwinds.com/checks/efficiency/" target="_blank">efficiency</a>,
            <a href="https://polaris.docs.fairwinds.com/checks/reliability/" target="_blank">reliability</a>
        </div>
        <div>{renderReport (get #checks confReport) }</div>
    |]

renderReport :: ReportChecks -> Html
renderReport checks =
  [hsx|
  {renderSummary (get #summary checks)}
  <h4>Pod Checks</h4>
  {renderPodChecks (get #podChecks checks)}
  <h4>Container Checks</h4>
  {forEach (toList (get #containerChecks checks)) renderContainerCheck }
|]

renderSummary :: ConfigCheckSummary -> Html
renderSummary summ =
  [hsx|
        <div class="table-responsive">
            <table class="table">
                    <tr>
                        <th>Summary</th>
                        <td>Danger : {get #dangerCount summ}</td>
                        <td>Warnings : {get #warningCount summ}</td>
                    </tr>
            </table>
        </div>
|]

renderPodChecks :: [ConfigCheck] -> Html
renderPodChecks cc =
  [hsx|
        <div class="table-responsive" style="width:98%; float:right">
            <table class="table">
                <thead>
                    <tr>
                      <th>category</th>
                      <th>checkID</th>
                      <th>severity</th>
                      <th>message</th>
                    </tr>
                  {forEach (filterPassing cc) renderConfigCheck }
                </thead>
            </table>
        </div>
|]

renderContainerCheck :: (Text, [ConfigCheck]) -> Html
renderContainerCheck (name, cc) =
  [hsx|
       <div>
        <p><em>{name}</em> : </p>
        <div class="table-responsive" style="width:98%; float:right">
            <table class="table">
                <thead>
                    <tr>
                      <th>category</th>
                      <th>checkID</th>
                      <th>severity</th>
                      <th>message</th>
                    </tr>
                </thead>
                {forEach (filterPassing cc) renderConfigCheck }
            </table>
        </div>
       </div>
|]

renderConfigCheck :: ConfigCheck -> Html
renderConfigCheck c =
  [hsx|
    <tr>
        <td>{get #category c}</td>
        <td>{get #checkID c}</td>
        <td>{get #severity c}</td>
        <td>{get #message c}</td>
    </tr>
|]
