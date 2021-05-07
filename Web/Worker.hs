module Web.Worker where

import IHP.Prelude
import Web.Types
import Generated.Types
import IHP.Job.Runner
import IHP.Job.Types

import Web.Job.PollCluster

instance Worker WebApplication where
    workers _ =
        [ worker @PollClusterJob
        -- Generator Marker
        ]
