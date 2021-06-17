module Web.View.ClusterConnections.Index where

import Web.View.Prelude

newtype IndexView = IndexView {clusterConnections :: [ClusterConnection]}

instance View IndexView where
  html IndexView {..} =
    [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={ClusterConnectionsAction}>ClusterConnections</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewClusterConnectionAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Friendly Name</th>
                        <th>URL</th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach clusterConnections renderClusterConnection}</tbody>
            </table>
        </div>
    |]

renderClusterConnection :: ClusterConnection -> Html
renderClusterConnection clusterConnection =
  [hsx|
    <tr>
        <td>{get #name clusterConnection}</td>
        <td>{get #url clusterConnection}</td>
        <td><a href={EditClusterConnectionAction (get #id clusterConnection)} class="text-muted">Edit</a></td>
        <td><a href={DeleteClusterConnectionAction (get #id clusterConnection)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
