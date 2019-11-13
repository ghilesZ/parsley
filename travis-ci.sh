#!/bin/bash -xue

bash -ex .travis-opam.sh
dune runtest
