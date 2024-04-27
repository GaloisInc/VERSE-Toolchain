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
