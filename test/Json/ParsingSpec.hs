{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ParsinSpec where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           GHC.Generics
import           IHP.Prelude
import           Test.Hspec

--
--getAuditReportJSON :: IO B.ByteString
--getAuditReportJSON = B.readFile "./test/Json/data/configReports.json"
--
--spec :: Spec
--spec =
--  it "User controller" $ do
--    d <- (eitherDecode <$> getAuditReportJSON) :: IO (Either String ConfReports)
--    case d of
--     Left err ->
--       putStrLn $ show err
--       --1 `shouldBe` 2
--     Right ps -> do
--       putStrLn $ show ps
--       items ps `shouldBe` []
--       --pure ()
