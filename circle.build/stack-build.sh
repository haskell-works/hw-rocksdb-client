#! /usr/bin/env sh

. $HOME/.nix-profile/etc/profile.d/nix.sh

stack setup
stack clean
stack build --test --no-run-tests
