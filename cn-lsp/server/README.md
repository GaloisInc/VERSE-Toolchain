# Building and Installing

Begin by installing OCaml and opam, if need be - here are
[instructions](https://ocaml.org/docs/installing-ocaml) for how to do so. This
build process has been tested with OCaml 4.14.1 and 5.1.1, though other versions
are likely to work as well.

With a choice of version in hand, the easiest way to build this package is with
a local opam switch, like so:
```sh
VERSION=4.14.1  # or 5.1.1
ln -s cnlsp.opam.locked-$VERSION cnlsp.opam.locked
opam switch create . ocaml.$VERSION --locked -y
eval $(opam env)
```

If this command succeeds, it should install the server into your local switch.
Any clients should run the binary from there. If a client is run outside the
context of your local switch, it will also need to set `$CERB_RUNTIME` to point
to Cerberus's runtime file dependencies, which are (currently) installed in the
switch at `<opam-dir>/lib/cerberus/runtime`.
