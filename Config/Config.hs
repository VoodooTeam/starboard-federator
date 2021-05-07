module Config where

import Control.Monad.Fail
import "cryptonite" Crypto.Cipher.AES (AES256)
import "cryptonite" Crypto.Cipher.Types (cipherInit)
import Crypto.Error (throwCryptoError)
import Data.ByteString.Base64
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.Prelude
import IHP.RouterSupport hiding (get)
import qualified System.Environment as Env
import Web.Types (AesKey (..))

config :: ConfigBuilder
config = do
  aesKey <- loadAesKey
  option aesKey
  env <- getEnvFromEnv
  option env
  option (AppHostname "localhost")

getEnvFromEnv :: MonadIO m => m Environment
getEnvFromEnv = do
  result <- liftIO $ Env.lookupEnv "ENV"
  case result of
    Just "dev" -> pure Development
    _ -> pure Production

-- we'll fail loudly if the key is not provided
loadAesKey :: (Control.Monad.Fail.MonadFail m, MonadIO m) => m AesKey
loadAesKey = do
  Just path <- liftIO $ Env.lookupEnv "AES_KEY_B64"
  let Right decoded = decode (fromString path)
  let key = throwCryptoError $ cipherInit decoded
  pure (AesKey key)
