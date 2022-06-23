#!/usr/bin/env bash

set -euxo pipefail

SCRIPT_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
readonly SCRIPT_DIR

PROJECT_ROOT="${SCRIPT_DIR}/.."
readonly PROJECT_ROOT

CERT_FILE="${PROJECT_ROOT}/test/certificate.pem"
readonly CERT_FILE

SFSIGNER_EXE="${PROJECT_ROOT}/build/sfsigner"

SFSIGNER_PRIVATE_KEY="$(cat "${PROJECT_ROOT}/test/private.key")"
export SFSIGNER_PRIVATE_KEY

for i in $(seq 20); do
  len=$((2**i))
  payload=$(mktemp)
  echo "Generate random payload of ${len} bytes in ${payload}"
  head -c "${len}" /dev/urandom > "${payload}"

  echo "Sign payload"
  sig=$(mktemp)

  "${SFSIGNER_EXE}" sign -c "${CERT_FILE}" "${payload}" > "${sig}"
  echo "Signature is written to ${sig}"

  echo "Verify payload against good signature"
  "${SFSIGNER_EXE}" verify "${payload}" -s "${sig}" -c "${CERT_FILE}"

  echo "Verify payload against bad signature"
  echo "A" >> "${payload}"
  if ! "${SFSIGNER_EXE}" verify "${payload}" -s "${sig}" -c "${CERT_FILE}"; then
    echo "Tampered payload failed signature verification as expected"
  else
    exit 1
  fi

  rm "${payload}" "${sig}"
done

unset SFSIGNER_PRIVATE_KEY