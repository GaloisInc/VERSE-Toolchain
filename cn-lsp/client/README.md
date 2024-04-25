# Building and Running

To run this extension, you'll need to open a VSCode window rooted in this
directory. From a terminal in that window:
- Run `npm install`
- Run `npm run compile` (or press Cmd-Shift-B)
- Press F5 to launch an extension host window
  - If this prompts you with a menu of choices, choose the one that mentions
    "Extension Development"

Currently, you'll see an error message appear saying that the client can't
connect to the server. This is expected, as we haven't yet linked it to a
server. The client-side functionality, namely highlighting, should work
regardless.
