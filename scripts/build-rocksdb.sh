#!/bin/bash

PRJ="$PWD"
DST="$PRJ/.rocksdb"

cd ~

_version=5.2.1

if [ ! -d ".rocksdb" ]; then
  git clone https://github.com/facebook/rocksdb "$DST"
fi

cd "$DST"


if ! ls librocksdb.${_version}.* 1> /dev/null 2>&1; then
  git pull
  git reset --hard v${_version}
  make -j4 static_lib
  make -j4 shared_lib
fi


OS="$(echo $(uname) | tr '[:upper:]' '[:lower:]')"
if [ "$OS" = "linux" ]; then
  sudo make INSTALL_PATH=/usr install
else
  sudo make install
fi
