# Building and Running

This application should be buildable with `cabal` version 3.10.3.0 and `ghc`
version 9.6.4. The easiest way to install these tools locally is via
[GHCup](https://www.haskell.org/ghcup/).

Run `install.sh` to build and install the application executable. This will make
the executable available in two places:
- In `bin/cn-lsp-server`
- In `~/.cabal/bin`

You should add `~/.cabal/bin` to your `$PATH` - this is the easiest way to let
installed clients locate find the executable they need. See [our client's
README](../client/README.md) for details on how it searches for a server
executable.

If all goes well, running the client should automatically start the server as a
child process.

The server requires a `cn` executable be available, and will fail to start
without one. If the environment variable `CN` is set, the server will treat its
contents as the path to a `cn` executable. If `CN` isn't set, it will look for
the first thing named `cn` on the user's `PATH`.
