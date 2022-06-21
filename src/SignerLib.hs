{-# LANGUAGE OverloadedStrings #-}
module SignerLib
    ( sha256Sum
    , fakeSign
    , smimeSign
    , smimeVerify
    , hasCommand
    ) where

import System.Environment
import System.IO
import Crypto.Hash
import qualified Data.ByteString.Lazy as L
import Turtle
import Data.Text as T
import Control.Exception
import OpenSSL.PKCS7
import OpenSSL.X509.Store
import OpenSSL.PEM
import OpenSSL.EVP.PKey
import OpenSSL.RSA
import Data.List.NonEmpty

sha256Sum :: Turtle.FilePath -> IO Text
sha256Sum f = do
  content <- L.readFile $ T.unpack (format fp f)
  let digest = hashlazy content :: Digest SHA256
  return $ fromString (show digest)

-- For testing purposes only
fakeSign :: Turtle.FilePath -> Shell Text
fakeSign f = liftIO $ sha256Sum f

hasCommand :: T.Text -> IO ()
hasCommand cmd = do
  result <- try (shells ("command -v " <> cmd <> ">/dev/null") Turtle.empty) :: IO (Either SomeException ())
  case result of
    Left ex -> die $ T.pack (show ex)
    Right s -> return ()

-- Sign file using X509 private key, obtained from an environment
-- variable SFSIGNER_PRIVATE_KEY. Due to the current HsOpenSSL
-- constraint, only RSA keys are supported.
smimeSign :: Turtle.FilePath -> Turtle.FilePath -> IO Text
smimeSign payload_file certificate_file = do
  payload <- readFile $ T.unpack  (format fp payload_file)
  certificate <- readFile (T.unpack (format fp certificate_file))
  certificate_x509 <- readX509 certificate
  privateKeyStr <- getEnv "SFSIGNER_PRIVATE_KEY"
  privateKey <- readPrivateKey privateKeyStr PwTTY
  sig_pkcs7 <- pkcs7Sign certificate_x509 privateKey [] payload [Pkcs7NoCerts, Pkcs7Detached, Pkcs7Binary, Pkcs7NoAttr]
  T.pack <$> writePkcs7 sig_pkcs7

smimeVerify :: Turtle.FilePath -> Turtle.FilePath -> Turtle.FilePath -> Maybe Turtle.FilePath -> IO Bool
smimeVerify content_file signature_file certificate_file cacert_file = do
  payload <-  readFile $ T.unpack  (format fp content_file)
  signature <- readFile (T.unpack  (format fp signature_file))
  signature_pkcs7 <- readPkcs7 signature
  certificate <- readFile (T.unpack (format fp certificate_file))
  certificate_x509 <- readX509 certificate
  store <- newX509Store
  case cacert_file of
    -- Skip chained verification
    Nothing -> verifyStatus <$> pkcs7Verify signature_pkcs7 [ certificate_x509 ] store (Just payload) [Pkcs7NoVerify]
    -- Perform chained verification
    Just x -> do
      cacert <- readFile (T.unpack (format fp x))
      cacert_x509 <- readX509 cacert
      addCertToStore store cacert_x509
      -- Flag set [Pkcs7NoIntern, Pkcs7NoChain] makes sure the signers'
      -- certificates (in this case just `certificate_x509`) and the
      -- verification chain (in this case just one CA certificate to
      -- attest signers' certificates) is in `store`
      verifyStatus <$> pkcs7Verify signature_pkcs7 [ certificate_x509 ] store (Just payload) [Pkcs7NoIntern , Pkcs7NoChain]

verifyStatus :: Pkcs7VerifyStatus -> Bool
verifyStatus (Pkcs7VerifySuccess (Just _)) = True
verifyStatus (Pkcs7VerifySuccess Nothing) = True
verifyStatus Pkcs7VerifyFailure = False
