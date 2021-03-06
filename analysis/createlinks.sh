#!/usr/bin/env bash

# Create links to all generated AG files.
# Allows Main.hs to be run with ghci.
# Run this script AFTER cabal build.
# The hs files will NOT automatically updated when cabal build is run unless you add new files.

rm src/GLua/AG/AST.hs
rm src/GLua/AG/PrettyPrint.hs
rm src/GLuanalysis/AG/ControlFlow.hs
rm src/GLuanalysis/AG/LiveVariables.hs

ln dist/build/gluanalysis/gluanalysis-tmp/GLua/AG/AST.hs src/GLua/AG/AST.hs
ln dist/build/gluanalysis/gluanalysis-tmp/GLua/AG/Token.hs src/GLua/AG/Token.hs
ln dist/build/gluanalysis/gluanalysis-tmp/GLua/AG/PrettyPrint.hs src/GLua/AG/PrettyPrint.hs
ln dist/build/gluanalysis/gluanalysis-tmp/GLuanalysis/AG/ControlFlow.hs src/GLuanalysis/AG/ControlFlow.hs
ln dist/build/gluanalysis/gluanalysis-tmp/GLuanalysis/AG/LiveVariables.hs src/GLuanalysis/AG/LiveVariables.hs
