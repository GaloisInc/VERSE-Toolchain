# Running

To run this extension for trial/development purposes, you'll need to open a
VSCode window rooted in this directory. From a terminal in that window:
- Run `npm install`
- Run `npm run compile` (or press Cmd-Shift-B)
- Press F5 to launch an extension host window
  - If this prompts you with a menu of choices, choose the one that mentions
    "Extension Development"


# Installing

## Installing a Release

See the [top-level README](../README.md) for instructions on installing the CN
client from a pre-built release.


## Building from Source

To install this extension in VSCode persistently, you can run the following from
a command line at this directory:
- `npm install`
- `npm run compile`
- `npm run dist`
  - This step will create a file with a `.vsix` extension
- `code --install-extension <filename>.vsix`

If you don't have `code` available at the command line, you can also accomplish
the last step from VSCode's GUI:
- Open the Extensions pane
- Click on the ellipsis in the top-right portion of the Extensions pane
- Select "Install from VSIX..."
- Choose the `.vsix` file that was created by `npm run dist`


# Running CN

Running CN requires that the client be able to find and run a CN language
server. [The top-level README](../README.md) has instructions for a basic
installation of our server involving a "local" [opam
switch](https://ocaml.org/docs/opam-switch-introduction).

The first time you launch the client, you'll automatically be prompted to select
the opam switch you created for the server's installation. Your choice is
preserved on a per-workspace basis. If you've previously pointed the client to a
switch, or to a server binary and Cerberus runtime files, and would like to
change your selection, you can open this extension's settings and either:
1.  (Recommended) Delete the existing paths for "Server Path" and "Cerb Runtime"
    and reload or restart VSCode window - you'll then be prompted to select a
    switch.
2.  Manually edit one or both of those two paths to point to whatever server and
    runtime files you want. These files will probably be at paths rooted in an
    opam switch. If the switch in question is local and you initialized it in
    `/path/to/dir`, the switch's path is `/path/to/dir/_opam`. If your switch is
    global and called `foo`, the switch's path should be `$HOME/.opam/foo`. You
    should only edit the switch portion of the paths


If you exit the choice prompt, the client will search these locations (in order)
for a server executable:
- The `CN_LSP_SERVER` environment variable
- On the current `PATH`, for an executable named `cn-lsp-server`

If the client can't find a server in one of these places, it will report an
error and fail to start.

To run CN on the currently-open file, open the command palette (Cmd-Shift-P) and
type "CN". You should see an option to run CN on the current file. If nothing is
wrong, a window should appear to tell you that. If something is wrong, you
should (hopefully) see CN errors rendered inline as red "squiggles", either in
the current file or in a file it depends on.

You can run CN on individual functions in a file, instead of on the file at
large, by clicking on the "code lens" that should appear above each function in
the file.

You can also choose to run CN on the current (`.c` or `.h`) file whenever it's
saved, by opening settings (Cmd-,), searching for "CN", and selecting the
checkbox for "Run On Save". You may not want to select this option if you're
working with files where running CN is expensive, because the ability to cancel
existing runs when new runs are requested hasn't (yet) been implemented, so you
may end up with a number of orphaned CN processes.

If the server fails to run CN or interpret its output, you can open up the
"Output" pane (Cmd-Shift-U) and select "CN" from the dropdown menu on the right
to see what output CN is producing and why the server is having trouble.


# Troubleshooting

The simplest way to get the client and/or server out of a bad state is to reload
the VSCode window. You can do this by opening the command palette (Cmd-Shift-P)
and typing "Reload Window". This should bring up a menu item titled "Developer:
Reload Window" - press enter to select it. The window will be reloaded and the
server will be restarted.


# Collecting Telemetry

Our CN language server supports optional collection of user telemetry, to
support improvements to the tool's usability. Collection is disabled by default.

To enable telemetry collection, go to the settings page for this extension and
provide a location in which you'd like telemetry to be stored. Changes to this
setting, whether to enable or disable collection or to change the storage
destination, will only take effect on the server's start/restart.
