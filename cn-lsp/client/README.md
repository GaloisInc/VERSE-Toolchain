# Running

To run this extension for trial/development purposes, you'll need to open a
VSCode window rooted in this directory. From a terminal in that window:
- Run `npm install`
- Run `npm run compile` (or press Cmd-Shift-B)
- Press F5 to launch an extension host window
  - If this prompts you with a menu of choices, choose the one that mentions
    "Extension Development"


# Installing

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
server. [Our server's README](../server/README.md) has instructions for a basic
installation. If you want to control the exact server the client uses, these are
the locations the client will search (in order) for a server executable:
- The `CN_LSP_SERVER` environment variable
- On the current `PATH`, for an executable named `cn-lsp-server`

If the client can't find a server in one of these places, it will report an
error and fail to start.

To run CN on the currently-open file, open the command palette (Cmd-Shift-P) and
type "CN". You should see an option to run CN on the current file. If nothing is
wrong, a window should appear to tell you that. If something is wrong, you
should (hopefully) see CN errors rendered inline as red "squiggles", either in
the current file or in a file it depends on.

You can also choose to run CN on the current (`.c` or `.h`) file whenever it's
saved, by opening settings (Cmd-,), searching for "CN", and selecting the
checkbox for "Run On Save". You may not want to select this option if you're
working with files where running CN is expensive, because the ability to cancel
existing runs when new runs are requested hasn't (yet) been implemented, so you
may end up with a number of orphaned CN processes.

If the server fails to run CN or interpret its output, you can open up the
"Output" pane (Cmd-Shift-U) and select "CN" from the dropdown menu on the right
to see what output CN is producing and why the server is having trouble.
