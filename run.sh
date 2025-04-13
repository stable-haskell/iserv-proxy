#!/usr/bin/env bash

# Simple script to build and start the proxy. The printed args show how to use
# it for testing a cross-compiler on the same machine.

set -e

GHC="${GHC:-ghc}"
port=5005

cabal build -w "$GHC" exe:iserv-proxy
echo ghc-args: -fexternal-interpreter -pgmi=$(cabal list-bin -w "$GHC" exe:iserv-proxy) -opti=127.0.0.1 -opti=$port
cabal run -w "$GHC" iserv-proxy-interpreter $port .
