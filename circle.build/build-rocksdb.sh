#!/bin/bash

cd ~

if [ ! -d "rocksdb" ]; then
  git clone https://github.com/facebook/rocksdb rocksdb
fi

cd rocksdb
git pull
git reset v5.2.1

make static_lib
sudo make install
