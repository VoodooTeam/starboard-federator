module Web.Routes where

import Generated.Types
import IHP.RouterPrelude
import Web.Types

-- Generator Marker

instance AutoRoute StaticController

instance AutoRoute ConfReportsController

instance AutoRoute ClusterConnectionsController

-- we don't use AutoRoute for healthCheck in order to set the route we want
instance CanRoute HealthCheckController where
  parseRoute' = string "/healthz" <* endOfInput >> pure HealthChecksAction

instance HasPath HealthCheckController where
  pathTo HealthChecksAction = "/healthz"
