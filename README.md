# vigger
External trigger based video capture

## Installing

### On Debian based systems

These instructions have been tested on Debian, Ubuntu and Raspberry Pi
OS.

Many of the dependencies are already in Debian repositories so let's
install them from there:

```sh
sudo apt install \
	cabal-install libghc-yaml-dev libghc-uuid-dev \
	libghc-optparse-applicative-dev libghc-curl-dev libghc-async-dev \
	libghc-hinotify-dev libghc-temporary-dev libghc-uri-encode-dev \
	libghc-filepath-bytestring-dev
cabal update
cabal install
```

If everything went well, it should install only `curl-aeson` from
Cabal and rest of the dependencies come from repos.

### Generic

Download and install all the dependencies:

```sh
cabal update
cabal install
```
