#!/usr/bin/env bash

set -e

GHC="${GHC:-ghc}"
port=5005

cabal build -w "$GHC" iserv-proxy
cabal run -w "$GHC" iserv-proxy-interpreter $port . &
interpreter=$?

proxy="$(cabal list-bin -w "$GHC" exe:iserv-proxy)"
echo "print \"Hello world!\"" | \
    "$GHC" \
    --interactive \
    -fexternal-interpreter \
    -pgmi "$proxy" \
    -opti 127.0.0.1 -opti$port

kill $interpreter
