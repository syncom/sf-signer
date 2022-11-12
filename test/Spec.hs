{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Text as T
import SignerLib
import Turtle
import Prelude hiding (FilePath)
import Control.Foldl as Fold
import System.Exit
import Control.Concurrent

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
  wrongCertificate:_ <- Turtle.fold makeTempFile Fold.list
  run $ writeTempFile wrongCertificate wrongCertificateStr
  caCertificate:_ <- Turtle.fold makeTempFile Fold.list
  run $ writeTempFile caCertificate caCertStr
  -- Skip chaining back to CA cert
  result0 <- run $ smimeVerify payload signature certificate Nothing
  result1 <- run $ smimeVerify payloadTampered signature certificate Nothing
  result2 <- run $ smimeVerify payload signature wrongCertificate Nothing
  assert result0
  assert (not result1)
  assert (not result2)
  -- Verify, chaining to CA cert
  resultChained0 <- run $ smimeVerify payload signature certificate (Just caCertificate)
  resultChained1 <- run $ smimeVerify payloadTampered signature certificate (Just caCertificate)
  resultChained2 <- run $ smimeVerify payload signature wrongCertificate (Just caCertificate)
  assert resultChained0
  assert (not resultChained1)
  assert (not resultChained2)

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

wrongCertificateStr :: String
wrongCertificateStr = "-----BEGIN CERTIFICATE-----\n\
  \MIIFOTCCAyGgAwIBAgIRAO85O5VwlXDP6epw6Cj01v0wDQYJKoZIhvcNAQELBQAw\n\
  \GDEWMBQGA1UEAxMNRXhhbXBsZVJvb3RDQTAeFw0yMjExMTEwNTE0MTlaFw0yMzEy\n\
  \MjAyMDEwNDdaMCUxIzAhBgNVBAMMGndyb25ndXNlckBzZnNpZ25lci5leGFtcGxl\n\
  \MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAvwW9vnx7jFRJuCkji0gL\n\
  \AtiBBcHMJfUgbRth+vcvnPivS0gS68iStR47XMK/lxgwhqhqJtYbIOd+Lt4+0AKf\n\
  \JH2der59chSkAyykivvj+1qLh745rabPtNWv60qVQlPsGrhyXOEPO/6VSmDDVWSE\n\
  \2FGKM3b0zMiDXJGsv6UkrnrD+Ru+vvQc+dKdgex7mCoep19qrCYqPeOA0mmNxdmY\n\
  \tbZS2Ni/5iFgrY0+7H9CuVfjycEkTmM9Ko3yQIXZVXFMcdL1VEfTf2T9vW1lvZml\n\
  \zNGeHLQM1eFVOi929koHpNf/JkR/qRBl2Eu+kWJoB78sFdb9H6DQfWKTXZa2lwwZ\n\
  \InGsoGDNX3FTjrbat5dhJvogcs0jyvIT5framSkJpeR1rmL/FRPWZY5R35OkyqNg\n\
  \HdnKPvbGKjENB36gVJWGuq6BX3bN3mq6ItXQ5xCr+k883KHBeYgiY8f67vPFZFKn\n\
  \bMI15WKqQXFB31EdXChiNpTOrxWu2nuLEWrbmAVaJ9IauA/MLGUZbA4FUDobgsmu\n\
  \AAG3rkA71Dl9fNEQRrixxanCgDRqR5chqFn90GbymqJKsFhvjJbzBSMXm74Nr8EV\n\
  \lBpE8hbmylMkS/RkUXIkDv3oSN6KYYNobg2KZlJR3kAAi7bnFenZRUuGpUTJHluP\n\
  \GTOwFMo39J1zPxLJGsGGMCcCAwEAAaNxMG8wDgYDVR0PAQH/BAQDAgO4MB0GA1Ud\n\
  \JQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjAdBgNVHQ4EFgQUGTagDuoW9KepsQNq\n\
  \sqTX1Bwcem4wHwYDVR0jBBgwFoAUqzxCUv1OjVhHFp+71Gzsj26YY2AwDQYJKoZI\n\
  \hvcNAQELBQADggIBACC1x9AlYw2urcC/Psb8jFG1kGbyrTcJP92QZCZRWP0dj0Yo\n\
  \MqNIkMCp9+GkIyxl/kfwSUQDNZof49yAUgPSckwIsRrBSb6LW/xdTAHGGvTDj++2\n\
  \WK3MuQzmhRPpZ+DFJhWhzm8cspK3lJw91zWkLnM9IBVh9pc3ZvYq02j42kjXqrv2\n\
  \xBI7tv7rJ965Gg9G1/ADHARvrJs/sPpiw+RcgPHBgFLTNaa2AXelZb7nR+UfJ1gt\n\
  \kpLQAJYyG0tW6cy7b0HWhU1V6aArfOrx3iLenLU6ULBcMnIeK3au6N1/UXPWLACo\n\
  \nn4y8qGrfpYmm0xw83zneVtsAlh9q5C0mUipbSynWt3bkz2x78H+BvubuIemkshr\n\
  \LQVG2mw+ptx+mjVKw/wN4+ytkatzXQC6h9MV0zj5FJNLDl32lceoR5siRZanbMeV\n\
  \F+RR3CMxcwY4v9PCUdSeHylt5ZHTT+CbiB/73v2hz7PIU5bto5j7uwS8fphdegWv\n\
  \rmGFb3TPnyoKninKVZCDBFImut3VZYfwz3WMhjONPLv8swQ/kHQXNeyvFJnOehGf\n\
  \IMTP3rtrADrVcJnenGtuObxp0JaI2Qnn21Zl1in6zEJ4jKNad5wGDY+yf9JHB/b+\n\
  \hyBz4DxE5VsE5FLRdQz4a9cQ5narweIfDsWAAPRKfVuKzVLlxzIfjXA1/nUs\n\
  \-----END CERTIFICATE-----"


caCertStr :: String
caCertStr = "-----BEGIN CERTIFICATE-----\n\
  \MIIE8DCCAtigAwIBAgIBATANBgkqhkiG9w0BAQsFADAYMRYwFAYDVQQDEw1FeGFt\n\
  \cGxlUm9vdENBMB4XDTIyMDYyMDIwMDA1MloXDTIzMTIyMDIwMTA0OFowGDEWMBQG\n\
  \A1UEAxMNRXhhbXBsZVJvb3RDQTCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\n\
  \ggIBAKNpPRor/OQXL+t0hCB+xrNBh1fR2NctXlrkSyS0edeNiqVGYVS6TrPgumf+\n\
  \WwmqLWiFRFMg/WD6F1e25sAtfshHDoy9qeT1+a2Ftu7Imap7Oo9wQrHa9s7uAfp+\n\
  \9DjAYLoszjS7hHoFRnJlUfbUqwcbCRrPqFU/6Twnek/ZO47B1+UvTGCeKrtVY7Jk\n\
  \UBcI8xmXIwWgdwtPkMENSkmec9IjGSatCDFGBUSKPiOjiiDpX+qL06pWcys1/pkd\n\
  \mc+JZPBOF/Vb2P6e+XTWNIuyO40CU9kHaWFL2zVnUN/r0pYLlZwcK4OjmYwmeJ7X\n\
  \FfBkC2DpljbQC8PYCmP60rzCKFQwX18E5DRDkcyf/iitG215LivEf/jSESK4nYcN\n\
  \Ni/7NeymE+cYMnEup6H7RgY7QS1oOt1vv9K5buwSIgNx4ioBtCP8is8SUMU8AUUM\n\
  \Idb6YpNQUQvTMVBvEvNFt93IqxJKMBzZFYgMNicoFXXg19zF/Wxr2LK8lfL9Xa9r\n\
  \Swccs4EFyAQLDL8NwiX7uwhFZHrUOP1AAksZOTzXHv660TVURrDLn62uk4yy49Ya\n\
  \IpH9hRwaqBBFXrvWZ/7yrwmcBxnSfwrqXjf6I7R2mtZmjWh/vnw5pitYkFlir0FI\n\
  \RTO7tLMpr1AlH066etq/vkQAm5kUTjxxdO5yF9R+CfERR4IJAgMBAAGjRTBDMA4G\n\
  \A1UdDwEB/wQEAwIBBjASBgNVHRMBAf8ECDAGAQH/AgEAMB0GA1UdDgQWBBSrPEJS\n\
  \/U6NWEcWn7vUbOyPbphjYDANBgkqhkiG9w0BAQsFAAOCAgEAHaMOTXhhomPVcKxr\n\
  \NhTsHG0urfl4lA+ZoIh3eE3r/xspT2vp/Xf96ZjbiLwfGMYGdR5MPeExtegPC8pX\n\
  \tPRDURbU8vC8VrtyxaohErRfmQZJurSdwlBkt9qxqQ4VuIGJg7/NsWc2GoYsH2Ki\n\
  \dq8LA91y9X79LgFqQMxOd87CvRoaW3wmKtk085r0dssPkgvCbbCmzdvbmjhbEhg6\n\
  \2FkADWDbp+keo+wlrInuTQodbuqizRVwlcpvQdG6KuAqnT1lrHh7wD0GK2MLilO9\n\
  \nd+gGIpfpD7ZdSKmWCgQW7eMWfMKAqZyGIF5ExQN+ahJkWbQlbrYQ8zSNZYuVRpe\n\
  \B3Wg/doo7qdSFx0tcl6DW3jxtxluPizYjxHMvTIJcRgwUZTEICkrcXgwmj/rAWMT\n\
  \KNG0Fegusjoja+ru7T6mNwg7yezpNNz6xttLsc83X69imuiiKNuBVUyoB5CXuux/\n\
  \xaYPusYbPs2OQhbhrepIokZhJqHen0sRohMQREIDEMH9Bx4iv7Wyr0QUKUvaX5fk\n\
  \UvzOUP69TTPTk3hQrrAbEQnae8lvAiFMx2SGzMI+kqjwHm2lizJNrJyvezUQqaew\n\
  \MfYA2CG52geIN9pZ4zwPglofNlQ9+fP7GlKZHNinkxbWy8RXUgBKTxF4PKVBoLoU\n\
  \/e5Drsz/fA+5qiBTaf6CyNkTBuY=\n\
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
