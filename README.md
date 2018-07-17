### Remember songs (for QPlaylist)

Copyright (C) 2017 Mark D. Blackwell.

## Overview

Provides a webpage for listeners
to send comments about recent songs to disk jockeys
for any radio station
which uses WideOrbit automation software.

## Development

For versions, see the following my-make* files.

I suggest that you *do not* use the '--global' flag.

Download and install the latest LTS (long-term support) version of Node.js, from its [website](https://nodejs.org/en/).

I'm using version 6.11.0.
(Earlier, I was using version 5.12.0.)

Node.js automatically comes with npm. I'm using version 3.10.10.
(Earlier, I was using version 3.8.6.)

Do these from the project root directory:

````bash
npm install uglify-js@{version}
npm install html-minifier@{version}
npm install cssnano-cli@{version}
./my-make
./my-make-other
````

## License

See license/none.txt
