# Building and Installing

Begin by installing OCaml and opam, if need be - here are
[instructions](https://ocaml.org/docs/installing-ocaml) for how to do so. CN and
Cerberus currently recommend, and build with, OCaml 4.14.1, and that version was
used to generate the lockfile which this installation process relies on. (I've
also been able to work with other versions as recent as 5.1.1, and have included
an alternate lockfile for 5.1.1 as well, but your mileage may vary.)

I recommend creating and using an opam switch to maintain an isolated dependency
installation and development environment:
```sh
opam switch create <switch-name> ocaml.4.14.1
eval $(opam env --switch=<switch-name> --set-switch)
```

Next, you need to install this project's dependencies. This project depends on
`cerberus-lib` and `cn`, which it will fetch and build from a particular commit
to their repository. However, if you have previously built and/or installed
`cerberus-lib` or `cn` locally, you may need to uninstall them before installing
this project's dependencies, or else your existing version may shadow the
version you're trying to install. This is more likely to happen with `cn`, which
is built and installed in the course of development on `cerberus`.
```sh
dune uninstall cn
opam remove cn
```

Now, install this project's dependencies:
```sh
opam install . --deps-only --locked -y
```

Now, you can build and install the project:
```sh
dune build
(cd bin && dune build)
dune install
```

Assuming you're using a switch, this will install a `cn-lsp-server` into
`$OPAM_SWITCH_PREFIX/bin`. I recommend manually expanding
`$OPAM_SWITCH_PREFIX/bin` and adding it to your `$PATH` in e.g. `.zshrc`, since
that's the easiest way for a client to locate a server binary. (The earlier
`opam env` command will have done this, but only for your current shell.)

If you're not using a switch, you'll need to say `dune install --bindir=$(pwd)`
instead, which will install a `cn-lsp-server` binary into the current directory.
You should still ensure the binary is available on your `$PATH`.
