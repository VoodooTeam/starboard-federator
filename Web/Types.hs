{-# LANGUAGE DeriveAnyClass #-}

module Web.Types where

import "cryptonite" Crypto.Cipher.AES (AES256)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, withObject, (.:))
import GHC.Generics
import Generated.Types
import IHP.ModelSupport
import IHP.Prelude

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data HealthCheckController
  = HealthChecksAction
  deriving (Eq, Show, Data)

data ClusterConnectionsController
  = ClusterConnectionsAction
  | NewClusterConnectionAction
  | CreateClusterConnectionAction
  | EditClusterConnectionAction {clusterConnectionId :: !(Id ClusterConnection)}
  | UpdateClusterConnectionAction {clusterConnectionId :: !(Id ClusterConnection)}
  | DeleteClusterConnectionAction {clusterConnectionId :: !(Id ClusterConnection)}
  deriving (Eq, Show, Data)

data ConfReportsController
  = ShowConfReportAction {confReportId :: !(Id ConfReport)}
  | ConfReportsByClusterAction
  | ConfReportsInClusterAction {clusterId :: !(Id ClusterConnection)}
  deriving (Eq, Show, Data)

newtype AesKey = AesKey AES256

-- K8sConfReport from k8s
newtype K8sConfReports = K8sConfReports
  {items :: [K8sConfReport]}
  deriving (Show, Generic, Eq, FromJSON)

data K8sConfReport = K8sConfReport
  { report :: ReportChecks,
    metadata :: Metadata
  }
  deriving (Show, Generic, Eq, FromJSON)

newtype Metadata = Metadata
  {labels :: MetaLabels}
  deriving (Show, Generic, Eq, FromJSON)

data MetaLabels = MetaLabels
  { resourceKind :: Text,
    resourceName :: Text,
    resourceNamespace :: Text
  }
  deriving (Show, Eq)

-- we have define our own FromJSON instance here because of the '.' in keys
instance FromJSON MetaLabels where
  parseJSON = withObject "MetaLabels" $ \obj -> do
    resourceKind <- obj .: "starboard.resource.kind"
    resourceName <- obj .: "starboard.resource.name"
    resourceNamespace <- obj .: "starboard.resource.namespace"
    pure (MetaLabels {..})

data ReportChecks = ReportChecks
  { containerChecks :: Map Text [ConfigCheck],
    podChecks :: [ConfigCheck],
    summary :: ConfigCheckSummary
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

data OwnerRef = OwnerRef
  { kind :: Text,
    name :: Text
  }
  deriving (Show, Generic, Eq, FromJSON)

data ConfigCheck = ConfigCheck
  { category :: Text,
    checkID :: Text,
    message :: Text,
    severity :: Text,
    success :: Bool
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)

data ConfigCheckSummary = ConfigCheckSummary
  { dangerCount :: Int,
    passCount :: Int,
    warningCount :: Int
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON)
