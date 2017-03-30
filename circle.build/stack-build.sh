#! /usr/bin/env sh

stack setup
stack clean
stack build --test --no-run-tests
