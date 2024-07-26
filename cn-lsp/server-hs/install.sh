#!/usr/bin/env bash

set -ex

BIN_NAME=cn-lsp-server
LOCAL_INSTALL_DIR=bin

cabal build exe:$BIN_NAME

mkdir -p $LOCAL_INSTALL_DIR
cabal install --overwrite-policy=always exe:$BIN_NAME
cabal install --overwrite-policy=always --installdir=$LOCAL_INSTALL_DIR exe:$BIN_NAME
