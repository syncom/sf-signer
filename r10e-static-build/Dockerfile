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

ENV SHN_COMMIT_SHA="bd66b86b72cff4479e1c76d5916a853c38d09837"
RUN nix-env -i git && \
    mkdir -p /build/static-haskell-nix && \
    cd static-haskell-nix && \
    git init && \
    git remote add origin https://github.com/nh2/static-haskell-nix.git && \
    git fetch --depth 1 origin ${SHN_COMMIT_SHA} && \
    git checkout FETCH_HEAD && \
    cd ../ && \
    mkdir -p /build/sf-signer

#########################################################
# Step 2: Copy files
#########################################################
COPY src/ /build/sf-signer/src
COPY app/ /build/sf-signer/app
COPY Setup.hs /build/sf-signer/Setup.hs
COPY build-options-dynamic.yaml /build/sf-signer/build-options.yaml
COPY package.yaml /build/sf-signer/package.yaml
COPY stack.yaml /build/sf-signer/stack.yaml
COPY stack.yaml.lock /build/sf-signer/stack.yaml.lock
COPY sf-signer.cabal /build/sf-signer/sf-signer.cabel
COPY r10e-static-build/default.nix /build/default.nix

# Build final artifact
RUN $(nix-build --no-link -A fullBuildScript \
  --argstr src-dir ${PWD}/sf-signer \
  --argstr package-name sf-signer \
  --argstr static-haskell-nix-dir ${PWD}/static-haskell-nix)
