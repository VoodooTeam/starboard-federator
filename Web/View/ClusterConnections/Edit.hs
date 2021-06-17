module Web.View.ClusterConnections.Edit where

import Web.View.Prelude

newtype EditView = EditView {clusterConnection :: ClusterConnection}

instance View EditView where
  html EditView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href="/">Home</a></li>
                <li class="breadcrumb-item"><a href={ClusterConnectionsAction}>ClustersConnections</a></li>
                <li class="breadcrumb-item active">Edit ClustersConnection</li>
            </ol>
        </nav>
        <h1>Edit ClustersConnection</h1>
        {renderForm clusterConnection}
    |]

renderForm :: ClusterConnection -> Html
renderForm clusterConnection =
  formFor
    clusterConnection
    [hsx|
    {(textField #name)}
    {(textField #url)}
    {(passwordField #saToken)}
    {submitButton}
|]
