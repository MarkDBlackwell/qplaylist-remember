### Remember songs (for QPlaylist)

Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.

## Overview

Provides a webpage for listeners
to send comments about recent songs to disk jockeys
for any radio station
which uses WideOrbit automation software.

## Development

Download and install Elm version 0.19.1 from its [website](http://elm-lang.org/) (try [here](http://github.com/elm-lang/elm-platform/releases)).

For versions of the other programs (and packages) known to work, see the following 'my-make*.sh' files.

Nevertheless, you should install the latest versions.

Do *not* use the '--global' flag.

Download and install the latest LTS (long-term support) version of Node.js from its [website](http://nodejs.org/en/).
The package provided by your Linux, etc. distribution probably will work.

Node.js automatically provides npm.

Do these, from the project root directory:

````bash
npm install elm-format
npm install uglify-js
npm install html-minifier
npm install cssnano-cli
./my-make.sh
./my-make-other.sh
````
Basically, ignore warnings.

## License

See license/none.txt
