# sf-signer: S/MIME File Signer

A simple file signing utility using X509 certificates so you don't have
to deal with `openssl`. Ubuntu Linux and MacOS are supported.

## For development

```bash
# Install stack
curl -sSL https://get.haskellstack.org/ | sh
# On Ubuntu install openssl headers
sudo apt install libssl-dev
# Build
make build
# Test
make test
# Clean
make clean
```

## How to use

1. You may use the prebuilt release binaries if you are on
`linux-x86_64` and `darwin-x86_64` platforms. Otherwise, you can always
build `sfsigner` from source (the generated artifect is
`build/sfsigner`).

   ```bash
   make build
   ```

   On Linux (Ubuntu), it's possible to build the `sfsigner` binary
   statically

   ```bash
   make static-build
   ```

2. Sign and verify files

   - Synopsis

     ```text
     A file signing utility with X.509 certificates

     Usage: sfsigner (version | sign | verify)

     Available options:
       -h,--help                Show this help text

     Available commands:
       version                  Show version
       sign                     Sign payload file.
         Private signing key is obtained from environment variable
         SFSIGNER_PRIVATE_KEY. Only RSA keys are supported currently.
       verify                   Verify signature of payload file
     ```

   - Example signing commands

     ```bash
     # Set environment variable SFSIGNER_PRIVATE_KEY for private key.
     # This is particularly useful in CI, where the environment variable
     # can be populated from a secrets manager
     export SFSIGNER_PRIVATE_KEY="$(cat /path/to/private.key)"

     # Print signature in stdout
     build/sfsigner sign /path/to/payload_file -c /path/to/certificate
     # Write signature to signature_file
     build/sfsigner sign /path/to/payload_file -c /path/to/certificate -o /path/to/signature_file
     ```

   - Example signature verification commands

     ```bash
     # Use test data. Should get "Verification: success"
     build/sfsigner verify test/payload.txt \
       --signature test/signature.pem \
       --cert test/certificate.pem \
       --cacert test/cacert.pem
     # If cacert is not present, skip chanined verification. Should get
     # "Verification: success"
     build/sfsigner verify test/payload.txt \
       --signature test/signature.pem \
       --cert test/certificate.pem
     ```

     Alternatively, signature verification can be done with `openssl smime`

     ```bash
     openssl smime -verify \
       -binary \
       -content test/payload.txt \
       -in test/signature.pem -inform PEM \
       -certfile test/certificate.pem \
       -noverify
     ```

     Currently, only RSA signing keys are supported. An example Public
     Key Infrastructure (PKI) can be found in [test/pki/](./test/pki/),
     where the CA certificates (`ExampleRootCA.crt`) and signer's
     certificate (`testuser_sfsigner.example.crt`) as well as their
     associated private keys, etc, were created using
     [certstrap](https://github.com/square/certstrap), with the
     following commands (and empty passwords)

     ```bash
     certstrap init --common-name "ExampleRootCA" --key-bits 4096
     certstrap request-cert --common-name "testuser@sfsigner.example" --key-bits 4096
     certstrap sign testuser_sfsigner.example --CA ExampleRootCA
     ```

## How to sign artifact in CI release flow

Please refer to [How to sign a release artifact in GitHub
Actions](docs/sign-artifact-in-gha.md) for a setup.
