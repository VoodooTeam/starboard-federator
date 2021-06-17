module Web.View.ClusterConnections.New where

import Web.View.Prelude

newtype NewView = NewView {clusterConnection :: ClusterConnection}

instance View NewView where
  html NewView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={ClusterConnectionsAction}>ClusterConnections</a></li>
                <li class="breadcrumb-item active">New ClusterConnection</li>
            </ol>
        </nav>
        <h1>New ClusterConnection</h1>
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
