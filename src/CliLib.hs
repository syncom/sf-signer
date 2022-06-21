{-# LANGUAGE OverloadedStrings #-}
module CliLib
    ( parser
    ) where

import Turtle
import Prelude hiding ( FilePath )
import Paths_sf_signer ( version )
import Data.Version ( showVersion )
import Control.Foldl as Fold ( list )
import Data.Maybe ( isJust )
import Data.Text as T
import Data.Text.IO as TIO ( putStr )
import SignerLib

parser :: Parser (IO ())
parser = versionParser <|> signParser <|> verifyParser

-- Subcommands

versionParser :: Parser (IO ())
versionParser =
  subcommand "version" "Show version" parseVersion

signParser :: Parser (IO ())
signParser =
  subcommand "sign" "Sign file" parseSign

verifyParser :: Parser (IO ())
verifyParser =
  subcommand "verify" "Verify signature" parseVerify

-- "version" subcommand options
version' :: IO ()
version' = putStrLn (showVersion version)

parseVersion :: Parser (IO ())
parseVersion = pure version'

-- "sign" subcommand options
data SettingsSignCmd = SettingsSignCmd
  { settingsSignCmdPayload     :: FilePath
  , settingsSignCmdCertificate :: FilePath
  , settingsSignCmdWriteSig    :: Maybe FilePath
  }

payloadParser :: Parser FilePath
payloadParser = argPath "payload" "The payload file to sign"

certificateParser :: Parser FilePath
certificateParser = optPath "cert" 'c' "Signer's X509 certificate in PEM"

writeSigParser :: Parser (Maybe FilePath)
writeSigParser = optional (optPath "output" 'o' "If present, write signature to a file")

settingsSignCmdParser :: Parser SettingsSignCmd
settingsSignCmdParser =
  SettingsSignCmd <$> payloadParser
                  <*> certificateParser
                  <*> writeSigParser

sign :: SettingsSignCmd -> IO ()
sign (SettingsSignCmd payload certificate outfile) = do
  sha256sum <- sha256Sum payload
  signature <- smimeSign payload certificate
  case outfile of
    Just x -> do
      writeTextFile x signature
      stderr ("Signature has been written to " <> inproc "echo" [ format fp x ] Turtle.empty)
    Nothing -> TIO.putStr signature

parseSign :: Parser (IO ())
parseSign = sign <$> settingsSignCmdParser

-- "verify" subcommand options
data SettingsVerifyCmd = SettingsVerifyCmd
  { settingsVerifyCmdPayload     :: FilePath
  , settingsVerifyCmdSignature   :: FilePath
  , settingsVerifyCmdCertificate :: FilePath
  , settingsVerifyCmdCACert      :: Maybe FilePath
  }

payloadParser' :: Parser FilePath
payloadParser' = optPath "payload" 'p' "The payload file to verify"

signatureParser :: Parser FilePath
signatureParser = optPath "signature" 's' "The PKCS#7 signature in PEM"

cacertParser :: Parser (Maybe FilePath)
cacertParser = optional (optPath "cacert" 't'
  "Trusted X509 CA certificate in PEM. When present, chained verification is performed. Otherwise, signer's certificate isn't verified.")

settingsVerifyCmdParser :: Parser SettingsVerifyCmd
settingsVerifyCmdParser =
  SettingsVerifyCmd <$> payloadParser'
                    <*> signatureParser
                    <*> certificateParser
                    <*> cacertParser

verify :: SettingsVerifyCmd -> IO ()
verify (SettingsVerifyCmd payload signature certificate cacert) = do
  result <- smimeVerify payload signature certificate cacert
  if result
    then echo "Verification: success"
  else
    die "Verification: failure"

parseVerify :: Parser (IO ())
parseVerify = verify <$> settingsVerifyCmdParser