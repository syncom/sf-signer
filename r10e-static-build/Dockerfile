# How to use
# cd .. && docker build -f r10e-static-build/Dockerfile -t shn-builder:test .
# The following information is from https://hub.docker.com/r/nixos/nix/tags
FROM nixos/nix:2.3.11@sha256:6903aa8484f625a304961ef9166ea89954421888f6a59af09893b17630225710 as image_builder

# For docker image prune filter
LABEL repo="sfsigner"

###############################################################
# Step 1: Check out nh2/static-haskell-nix
###############################################################
WORKDIR /build

# NIXPKGS_COMMIT_SHA should match nixpkgs revision pinned at SHN_COMMIT_SHA of
# static-haskell-nix
ENV NIXPKGS_COMMIT_SHA="d00b5a5fa6fe8bdf7005abb06c46ae0245aec8b5"
ENV SHN_COMMIT_SHA="75658b06789f6bd7c7aee26044dc60c0f13e9166"
RUN nix-env -i git && \
    mkdir -p /build/static-haskell-nix && \
    cd /build/static-haskell-nix && \
    git init && \
    git remote add origin https://github.com/syncom/static-haskell-nix.git && \
    git fetch --depth 1 origin ${SHN_COMMIT_SHA} && \
    git checkout FETCH_HEAD && \
    mkdir -p /build/nixpkgs && \
    cd /build/nixpkgs && \
    git init && \
    git remote add origin https://github.com/NixOS/nixpkgs.git && \
    git fetch --depth 1 origin ${NIXPKGS_COMMIT_SHA} && \
    git checkout FETCH_HEAD && \
    mkdir -p /build/sf-signer && \
    cd /build

#ENV NIX_PATH=nixpkgs=/build/nixpkgs
ENV NIX_PATH=
#########################################################
# Step 2: Copy files
#########################################################
COPY src/ /build/sf-signer/src
COPY app/ /build/sf-signer/app
COPY test/ /build/sf-signer/test
COPY Setup.hs /build/sf-signer/Setup.hs
COPY README.md /build/sf-signer/README.md
COPY build-options-dynamic.yaml /build/sf-signer/build-options.yaml
COPY package.yaml /build/sf-signer/package.yaml
COPY stack.yaml /build/sf-signer/stack.yaml
COPY stack.yaml.lock /build/sf-signer/stack.yaml.lock
COPY sf-signer.cabal /build/sf-signer/sf-signer.cabel
COPY r10e-static-build/default.nix /build/default.nix

#########################################################
# Step 3: Build final artifact
#########################################################

RUN BUILD_SCRIPT=$(nix-build --no-link -A fullBuildScript \
  -j auto \
  --argstr src-dir ${PWD}/sf-signer \
  --argstr package-name sf-signer \
  --argstr static-haskell-nix-dir ${PWD}/static-haskell-nix) && \
  echo ${BUILD_SCRIPT} && exit 0 && \
  ${BUILD_SCRIPT}
