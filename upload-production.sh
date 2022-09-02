#!/bin/sh -e

# Copyright (C) 2022 Mark D. Blackwell.
#   All rights reserved.
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# The path to the directory containing this script (without a trailing separator):
script_directory="$( cd "$( dirname $0 )" && echo $PWD )"

#-------------
cd $script_directory

src/upload-common.sh

#-------------
echo "When prompted, enter the password for the production server."

cat src/session.ftp | /usr/bin/ftp -n `cat var/domain-name-production`

echo "Success"
