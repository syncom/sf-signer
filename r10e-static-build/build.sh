#!/usr/bin/env bash

set -euxo pipefail

SCRIPT_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
OUT_DIR="${SCRIPT_DIR}/out"
REVISION=$(git --work-tree="${SCRIPT_DIR}/../" \
  --git-dir="${SCRIPT_DIR}/../.git" \
  rev-parse HEAD)
IN_NAME="sfsigner"
OUT_NAME="sfsigner-r10e"
BUILDER_TAG_NAME="sfsigner-builder:$REVISION"

echo "Building statically linked sfsigner"
cd "${SCRIPT_DIR}/.."
docker build -f "${SCRIPT_DIR}/Dockerfile" -t "${BUILDER_TAG_NAME}" .
docker images "${BUILDER_TAG_NAME}"
mkdir -p "${OUT_DIR}"

docker run --entrypoint=/bin/sh --rm -i -v "${OUT_DIR}":/tmp/ \
  "${BUILDER_TAG_NAME}" << CMD
cp -Lr "/build/result/bin/${IN_NAME}" "/tmp/${OUT_NAME}"
CMD

echo
echo "============ ARTIFACT INFO ============"
echo "Artifact created in ${OUT_DIR}/${OUT_NAME}"
echo -n "Artifact sha256sum: "
shasum -a 256 "${OUT_DIR}/${OUT_NAME}" | cut -f1 -d' '
echo