#! /usr/bin/env sh

stack setup
stack clean
LD_LIBRARY_PATH=/usr/local/lib stack build --test --no-run-tests
