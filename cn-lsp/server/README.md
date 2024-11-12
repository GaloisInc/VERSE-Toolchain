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

If this command succeeds, it should put a `cn-lsp-server` binary on your
`$PATH`, but (because the switch is local) only when you're in this directory. I
recommend choosing a location that's always available on your `$PATH` and either
copying `cn-lsp-server` to that location or symlinking to it:
```sh
cp `which cn-lsp-server` /somewhere/on/PATH
# or
ln -s `which cn-lsp-server` /somewhere/on/PATH
```
