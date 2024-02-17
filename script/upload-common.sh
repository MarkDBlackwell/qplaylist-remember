#!/bin/sh -e

# Copyright (C) 2022 Mark D. Blackwell.
#   All rights reserved.
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# The path to the directory containing this script (without a trailing separator):
script_directory="$( cd "$( dirname $0 )" && echo $PWD )"

#-------------
cd $script_directory/..

./clean.sh

mkdir develop-tmp

#-------------
cp --target-directory=develop-tmp \
  build/Main-min.js \
  build/RememberSongsDevelop.html \
  build/RememberSongs-min.css \
  build/RememberSongs-responsive-min.css \
  build/RememberSongs-windows-phone-8-viewport-fix-min.js \
  build/RememberSongs.html \
  build/SetUp-min.js \
    script/install-qplaylist-remember.sh

cd develop-tmp

/usr/bin/tar --create --file all.tar *
