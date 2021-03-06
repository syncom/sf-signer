{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Text as T
import SignerLib
import Turtle
import Prelude hiding (FilePath)
import Control.Foldl as Fold
import System.Exit

-- Cross check on non-null payload
prop_sha256CrossCheck :: String -> Property
-- Example of pre-condition. We don't need it for this particular test
--prop_sha256CrossCheck payload = not (Prelude.null payload) ==> monadicIO test
prop_sha256CrossCheck payload = monadicIO test
  where test = do path:_ <- Turtle.fold makeTempFile Fold.list
                  run $ writeTempFile path payload
                  sum1:_  <- Turtle.fold (fakeSign path) Fold.list
                  sum2':_ <- Turtle.fold (sha256Sum' path) Fold.list
                  let sum2 = lineToText sum2'
                  rm path
                  assert (sum1 == T.take 64 sum2)

-- Cross check on null payload
prop_sha256OfNull :: Property
prop_sha256OfNull = monadicIO $ do
  nullsum1:_ <-  Turtle.fold (fakeSign "/dev/null") Fold.list
  assert $ nullsum1 == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

-- Signature verification check
prop_smimeVerify :: Property
prop_smimeVerify = monadicIO $ do
  payload:_ <- Turtle.fold makeTempFile Fold.list
  run $ writeTempFile payload payloadStr
  payloadTampered:_ <- Turtle.fold makeTempFile Fold.list
  run $ writeTempFile payloadTampered payloadStrTampered
  signature:_ <- Turtle.fold makeTempFile Fold.list
  run $ writeTempFile signature signatureStr
  certificate:_ <- Turtle.fold makeTempFile Fold.list
  run $ writeTempFile certificate certificateStr
  result0 <- run $ smimeVerify payload signature certificate Nothing
  result1 <- run $ smimeVerify payloadTampered signature certificate Nothing
  assert result0
  assert (not result1)

payloadStr :: String
payloadStr = "The answer is 42."

payloadStrTampered :: String
payloadStrTampered = "The answer is not 43."

signatureStr :: String
signatureStr = "-----BEGIN PKCS7-----\n\
  \MIICkAYJKoZIhvcNAQcCoIICgTCCAn0CAQExDzANBglghkgBZQMEAgEFADALBgkq\n\
  \hkiG9w0BBwExggJYMIICVAIBATAtMBgxFjAUBgNVBAMTDUV4YW1wbGVSb290Q0EC\n\
  \EQD03VgrkNHOZbEUlZlOcWjsMA0GCWCGSAFlAwQCAQUAMA0GCSqGSIb3DQEBAQUA\n\
  \BIICAB+MnYxYXUl7HD6gFJQz/s9gIs7cHzHPJPmUI9rvpwn0PzrfbMa1QcEFzEUK\n\
  \/TVMmR0Dbbz0VxEFgpfCN6lxvcq3VmZ5OhUBXAsTDt3vJYw5F5DURxSZ04AfkLGY\n\
  \BaXlz8TEr1P+4h1okjuGhvEsLP6jyT8VYnkpFkZyYEk1baUr2zEqezs5gO2TtEBg\n\
  \SfXfeH95uLg0qP9cDYuJCE4ix81ELlc58OsCiLuWVlJV/azfgqquuNKYFce4BXeg\n\
  \SqXwRgeMhjW1jzlEGxTwG7h4UNEmiQdViDZN4GpP7mR9Zczu7sbwEFi8TagKP8YV\n\
  \52vWlUH2OJwEFr5zaVNMcTO86gMcs8dEFBljiHPOA0nfrrKxVwOZ1vzI8lzuC3RL\n\
  \sIPkJBuO3gUrrw6uJoDCCjwY5rRlZaYE7xYQDCnPCgwh+G3OIy8TX7kwPd8N/uRN\n\
  \U4uWx8iMed8vJjl+SXOD4YNB4SxjVDtpLfYshgtJKhF8SKl0h2O2zs22uIA+f04V\n\
  \a4k9bL3h9mdpgdau1of+Mik4sKcHaVTykDYSgWhbzM8neUuFq3zjP59SbhWAHW7q\n\
  \kbyzf1TlGHHKmS0xmXTSRMQH4x2dL2aFKPjFcNrLtZ6V3B/jwphtiDCQGR39Fip0\n\
  \usWTauDmk1hwXatEKxuwQvRcZ2HKLRs9Io6W4j3CkcwCjWJJ\n\
  \-----END PKCS7-----"
  
certificateStr :: String
certificateStr = "-----BEGIN CERTIFICATE-----\n\
  \MIIFODCCAyCgAwIBAgIRAPTdWCuQ0c5lsRSVmU5xaOwwDQYJKoZIhvcNAQELBQAw\n\
  \GDEWMBQGA1UEAxMNRXhhbXBsZVJvb3RDQTAeFw0yMjA2MjAyMDAzMzJaFw0yMzEy\n\
  \MjAyMDEwNDdaMCQxIjAgBgNVBAMMGXRlc3R1c2VyQHNmc2lnbmVyLmV4YW1wbGUw\n\
  \ggIiMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQC/Sm69Lce4RyQsgKdmWNLZ\n\
  \WJjfnJoJATvBDq+swOrWW8Z9i/gKQLKaQ2Tt7jU77xfQF/lyxlxO9SYWuApXIOZr\n\
  \RSXQaBFhRL6WjbSk+f93ZQEd74PIIo3N31dDElFdF6L+e+mjgttuYBrX3mqSSht3\n\
  \NhN3ze1I7VW2J/6bAy48LBqjWJZk/A4q/cpPC8PHpAGDZyAOsMCKUOaJAulRPxI+\n\
  \M2Poqvwn2p2QjCCc5c2Haw0sYfnoVNRyFPO69MOYs8hmNIvSa5iwMszh7Y2w5pZA\n\
  \mEBG3J8TThu77qtgNMzImKDTAwB42pAgyI2TZLczqU/vELbOwnmG8QO8aslSwhKT\n\
  \gJPMQB9yTzGvri1d903lIZPEr4X4FjnT/YkbKlB39b+O+l4wtbL2mkp8oJ00w2Iy\n\
  \xuRzW8CAxaWhIwaY87QxAug1Cm4A6mFpwd6XU81XfSrh1wMl0t9wUSxB3gpd2mvp\n\
  \y0wx+3sfCMTnlkhs4epS/wqNcltKm03Eu40IOc+H5f39hi0bPjN0e/YyZvgbLEVb\n\
  \Zmj1vTMnXamCsCbSBTM1ujKJLwQZJSgeYQGzY2tmVbE/R699N0cxt9NdlCeXRzJp\n\
  \jn4+3bZ3qHYUOxLRkmaM7LbJbDzylsBmdvvwUImJPlZgm1tjo05z+BzoCHLblkT6\n\
  \/UkoFKRTivOA272xawHtaQIDAQABo3EwbzAOBgNVHQ8BAf8EBAMCA7gwHQYDVR0l\n\
  \BBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMB0GA1UdDgQWBBRiXYhIdsBNqCyYu9zc\n\
  \Uc9HMA1kBDAfBgNVHSMEGDAWgBSrPEJS/U6NWEcWn7vUbOyPbphjYDANBgkqhkiG\n\
  \9w0BAQsFAAOCAgEAVIVzIo/IKavHW79xQhui5zTUwFrDuGwLMitlA8U8ya0wPpYY\n\
  \698fbMDR+SMDhm0rbhVGBHVBFSlev3Z/mMhDia/GQieKO4jTW2B5hnQlQ3MHmdgj\n\
  \8Tdg8LoDT0bxIQj4jcJU7633YFdJinZcpnKStIx9eLXeIJ3yuo5Fdr4ptrU713dO\n\
  \e6J+e1KclC2gCGLIs7FBN6mQUoFz7DDBw2klE9YOCHQm7PrsA9Bj6VAC/Q8RY1CH\n\
  \3J8Rp9LuIUuQA+TpI/SuMoWdm1w9rVkx0q6jvUEn+knIlqBeG+LALuugYRXSXJVW\n\
  \weUzyyMn8kbNhH1v+KJrmI6w6R4j9TI2dtfQJXF6vFV1AV79tze0ChdQB3e9frdg\n\
  \As7fryqiDZi27tKzf6NFvJJrvEbTel8nzFz5153wzn56WJvXeYGffVUZOKTm3Z3c\n\
  \JylXAoBnIkxH8nr17Vjof8Aq9c3umVVgdoGoEbWd72G55RlxatysbUatS5qsp5in\n\
  \dB2XAGJtUYsSvfdVADunaw+zwgoqm9mFF2YBruq0E8ccKMg+uAEhkqpa8uKHoF0P\n\
  \IqK4kmL5mZVdvE5IrVQ/CU81bvq8MIKPPrXxbGQkDX5mGgN9EtLsbsYJZpgSr4zj\n\
  \RGItirRguhphioaYGsi5teGPgCyvMr3W+AWS7XFg/RE8QvZI5gZFi9EZobQ=\n\
  \-----END CERTIFICATE-----"

sha256Sum' :: Turtle.FilePath -> Shell Line
sha256Sum' f = inproc "sha256sum" [ format fp f ] Turtle.empty

makeTempFile :: Shell FilePath
makeTempFile = do
    (path, _) <- using (mktemp "/tmp" "quickcheck")
    return path

writeTempFile :: FilePath -> String -> IO ()
writeTempFile fpath text = Turtle.output fpath (inproc "echo" [T.pack text] Turtle.empty)

selfCheckTest :: IO ()
selfCheckTest = do
  let cmd = "sha256sum"
  hasCommand cmd

main :: IO ()
main = do
  putStrLn "[program self checking]"
  selfCheckTest
  putStrLn "[program self check passed]"
  putStrLn "\n[cross checking sha256 sums]"
  result1 <- quickCheckWithResult stdArgs { maxSuccess = 314 } prop_sha256CrossCheck
  putStrLn "[checking sha256 of empty data]"
  result2 <- quickCheckWithResult stdArgs { maxSuccess = 1 } prop_sha256OfNull
  putStrLn "[checking signature verification]"
  result3 <- quickCheckWithResult stdArgs { maxSuccess = 1 } prop_smimeVerify
  unless (isSuccess result1 && isSuccess result2 && isSuccess result3) exitFailure
  putStrLn "All done!"
