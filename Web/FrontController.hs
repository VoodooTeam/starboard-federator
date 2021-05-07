module Web.FrontController where

import           IHP.RouterPrelude
import           Web.Controller.Prelude
import           Web.View.Layout         (defaultLayout)

import Web.Controller.ClusterConnections
import Web.Controller.ConfReports
import           Web.Controller.Static

instance FrontController WebApplication where
    controllers =
        [ startPage ConfReportsByClusterAction
        -- Generator Marker
        , parseRoute @ClusterConnectionsController
        , parseRoute @ConfReportsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
