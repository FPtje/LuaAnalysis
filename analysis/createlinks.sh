#!/usr/bin/env bash

# Create links to all generated AG files.
# Allows Main.hs to be run with ghci.
# Run this script AFTER cabal build.
# The hs files will NOT automatically updated when cabal build is run unless you add new files.

rm src/GLua/AG/AST.hs
rm src/GLua/AG/PrettyPrint.hs
rm src/GLuanalysis/AG/ControlFlow.hs

ln dist/build/luanalysis/luanalysis-tmp/GLua/AG/AST.hs src/GLua/AG/AST.hs
ln dist/build/luanalysis/luanalysis-tmp/GLua/AG/PrettyPrint.hs src/GLua/AG/PrettyPrint.hs
ln dist/build/luanalysis/luanalysis-tmp/GLuanalysis/AG/ControlFlow.hs src/GLuanalysis/AG/ControlFlow.hs
