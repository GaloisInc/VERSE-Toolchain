# coq-synthesis-vscode

VSCode plugin for synthesizing Coq proofs using proverbot9001


## Dependencies

This plugin uses [proverbot9001](https://github.com/GaloisInc/proverbot9001),
which must be installed separately and has dependencies of its own.

Instructions below are based on
[the Proverbot readme](https://github.com/GaloisInc/proverbot9001/blob/master/README.md)
and [setup script](https://github.com/GaloisInc/proverbot9001/blob/master/src/setup.sh),
but are reduced to just the parts relevant for the plugin.

Proverbot setup:

* Clone the repo and submodules:

  ```sh
  git clone https://github.com/GaloisInc/proverbot9001
  cd proverbot9001
  git submodule update --init dataloader/gestalt-ratio/
  ```

* Set up an opam switch with Coq 8.16.1 and `coq-serapi`:

  ```sh
  # Create a new switch named `proverbot` with ocaml 4.13.1
  opam update
  opam switch create proverbot 4.13.1
  eval $(OPAMSWITCH=proverbot opam env)
  opam pin add coq 8.16.1
  opam pin add menhir 20190626
  opam install coq menhir coq-serapi
  ```

* Install the Rust nightly toolchain, which is used to compile one of the
  Python dependencies:

  ```sh
  # Instructions copied from https://rustup.rs/:
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

  rustup toolchain install nightly
  ```

* Install libgraphviz development headers:

  ```sh
  # For Debian/Ubuntu:
  sudo apt install libgraphviz-dev
  ```

  If the headers are missing, you may get the following error while installing
  Python dependencies:

  ```
  pygraphviz/graphviz_wrap.c:2711:10: fatal error: graphviz/cgraph.h: No such file or directory
  ```

* In the `proverbot9001` directory, create a Python virtualenv containing the
  Python dependencies:

  ```sh
  cd proverbot9001
  virtualenv venv
  . venv/bin/activate
  pip install -r requirements.txt
  pip install -e coq_serapy/
  (cd dataloader/dataloader-core/ && maturin develop --release)
  ```

* Download weights:

  ```sh
  cd proverbot9001
  make download-weights
  ```


## Installing the plugin

* Build the `.vsix` package:

  ```sh
  cd proof-synthesis-vscode
  # Install node.js dependencies
  npm install
  # Build the package
  npm exec vsce -- package
  ```

  This will create a file named `coq-synthesis-vscode-${version}.vsix

* In the VSCode command palette, run "Extensions: Install from VSIX...", then
  choose the `.vsix` file created in the previous step.

* Adjust settings to let the extension know where to find your `proverbot9001`
  checkout.  In the VSCode command palette, run "Preferences: Open Settings
  (UI)" to open the settings editor.  In the editor, search for `proverbot`.
  Find the setting `Coq-synthesis-vscode > Proverbot9001: Path` and set it to
  the absolute path to your proverbot9001 checkout.  (Note that tilde expansion
  for home directories does not apply here - it must be the full path, e.g.
  `/home/username/proverbot9001`.)

* If you have other opam switches aside from the `proverbot` switch created
  above, either ensure `proverbot` is the active switch each time you use this
  plugin, or set the `Proverbot9001: Opam Switch` setting to `proverbot`.


## Using the plugin

In the VSCode editor, open a Coq file, put the cursor on a proof, then open the
command palette and run the "Synthesize Proof" command.  This will open a new
terminal running the `proverbot9001` scripts.  If synthesis successfully
produces a proof, the plugin will insert the new proof into the file, replacing
the existing proof (and changing `Admitted.` to `Qed.` if needed).  Also, on
success or failure, the plugin will display an interactive view of the proof
search tree in a side pane.

To cancel a long-running proof synthesis attempt, click on the terminal where
the `proverbot9001` scripts are running and press Ctrl-C.
