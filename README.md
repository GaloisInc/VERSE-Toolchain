# The VERSE Development Toolchain

This repository assembles the core technologies that make up the VERSE TA1
toolchain and packages them into IDE and CI/CD integrations.

This is a companion to the [VERSE Open SUT
repository](https://github.com/GaloisInc/VERSE-OpenSUT) for TA2.

[ [VERSE project proposal](https://drive.google.com/drive/u/0/folders/1S6wk-aXLZh_dNGU0IcKxB2tnXe5zjV1C) ]

## Toolchain Components

The VERSE toolchain brings together core technologies from EPFL, the University
of Cambridge, the University of Illinois at Urbana-Champaign, the University of
Maryland, the University of Massachusetts Amherst, and the University of
Pennsylvania.

### CN

Developers write inline specifications alongside their C code using the CN
specification language. Additionally, CN's automated verification backend
attempts to automatically prove specifications.

[ [Homepage](https://www.cl.cam.ac.uk/~cp526/popl23.html),
[paper](https://www.cl.cam.ac.uk/~cp526/popl23.pdf),
[GitHub](https://github.com/rems-project/cerberus/tree/master/backend/cn),
[manual](https://github.com/rems-project/cerberus/tree/master/backend/cn/manual) ]

### Runtime Specification Testing

Runtime spec testing is made up of two parts:

1. Synthesizing test input from specifications, a form of property-based testing (PBT).
1. Transliterating CN specifications into C runtime tests that check whether the spec holds for a specific program execution.

TBD: Links and details as these projects progress.

See [Tyche](https://github.com/tyche-pbt/tyche-extension) and QuickChick ([manual](https://softwarefoundations.cis.upenn.edu/qc-current/index.html), [GitHub](https://github.com/QuickChick/QuickChick)) for inspiration.

### Proof Synthesis, Repair and Visualization

CN provides an escape hatch to Roq (formerly Coq) to provide manual proofs of properties that cannot be automatically verified.

- **Proof synthesis** attempts to automatically discharge these proof obligations.
- **Proof repair** attempts to automatically fix existing proofs if the code
  associated with them changes.
- **Proof visualization** assists the manual proof effort by tracking and
  visualizing complex C memory states and other proof details.

TBD: Links to project pages, more details as we discover them.

## Installation and Use

Below are instructions for installing the VSCode integrations available for CN.
Note that these integrations rely on
[z3](https://github.com/Z3Prover/z3/releases) and [gmp](https://gmplib.org/)
being installed and available on your `$PATH`; install them via your preferred
package manager. For example, for macOS we recommend running `brew install z3
gmp`.

### Installing a Release

Note: this method of installation is **experimental**. If you encounter issues
with it, please report them, and in the meantime use the below instructions on
building from source as a fallback.

We aim to build and package our VSCode integrations for multiple operating
systems through this repo's CI and to make them available periodically as
["Releases"](https://github.com/GaloisInc/VERSE-Toolchain/releases). To install
these integrations from a release:
- Download and unzip the release associated with your OS, or the OS nearest
  yours, to produce a `cn-client-X.Y.Z.vsix` file.
- Run `code --install-extension /path/to/cn-client-X.Y.Z.vsix`.
- If you last installed this extension by building it from source, open this
  extension's settings in VSCode and delete any configured "Server Path" and
  "Cerb Runtime" entries. (Otherwise, the client will not pick up the server
  binary and runtime files included in the CI-built release.) You don't need to
  add new entries; the client will do this automatically when you restart
  VSCode.
- Restart VSCode.

Now, if you open a C file in VSCode, you should be able to use CN features.


### Building from Source

Begin by installing OCaml and opam, if need be - here are
[instructions](https://ocaml.org/docs/installing-ocaml) for how to do so. This
build process relies on using a local opam switch based on OCaml 5.1.1. (Prior
versions of the server supported earlier versions of OCaml, but the current one
does not.)

(Note: this server has not been regularly built or tested on Windows; these
instructions assume you're using Linux or macOS.)

If you're building from scratch, you can create such a switch and build and
install these packages in it with:
```sh
opam update
opam switch create . ocaml.5.1.1 --locked -y
eval $(opam env)
```

If you've previously gone through this switch-initialization process, you should
be able to activate the existing local switch and rebuild the server most easily
with `dune`:
```sh
opam switch .
eval $(opam env)
dune build
dune install
```

If this command succeeds, it should install the `cn-lsp` and `telemetry`
packages into your local switch.

Now that you've installed the language _server_, you'll also need to install a
language _client_, if you haven't already. For instructions on how to build and
install our language client for VSCode, see
[cn-client/README.md](cn-client/README.md). This step isn't required when
installing a release because the server and client are packaged together in the
release, and both are installed at once.

Once you've done that, if you open a C file in VSCode, you should be able to use
CN features.


### How to run CN

You need (1) `git` and (2) `docker` (or a comparable alternative such as `colima`).

In the root directory invoke `make`.

## How to Contribute

- Review the [code of conduct](CONDUCT.md) and [developer guidelines](CONTRIBUTING.md).
- Check out the [reading list](docs/reading_list.md).
- Get some experience running CN on [simple examples](cn-intro).
