{-# LANGUAGE DeriveAnyClass #-}

module Web.Controller.HealthCheck where

import Web.Controller.Prelude
import Data.Aeson
import GHC.Generics

newtype IndexView' = IndexView' {healthy :: Bool}
  deriving (Show, Generic, Eq, ToJSON)

instance Controller HealthCheckController where
    action HealthChecksAction = do
        _ <- query @ClusterConnection |> fetchCount
        let ok = IndexView' {healthy=True}
        renderJson $ toJSON ok


