#!/bin/sh

test_dir=$(dirname $0)
cd $test_dir

cabal clean &&
cabal configure --disable-optimization &&
cabal build &&
./dist/build/testsuite/testsuite
