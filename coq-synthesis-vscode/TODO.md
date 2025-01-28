Some ideas for improving the plugin:

- Currently, the viewport size for the search tree display is set when the
  webview panel is first opened, and resizing the webview panel doesn't resize
  the viewport.  This means making the panel bigger doesn't actually let you
  see more of the tree at once.  We should add a resize handler that keeps the
  viewport size in sync with the size of the webview.
- When the Python invocation fails (exits nonzero), we keep the terminal open
  so the user can see the error.  Currently, the terminal stays open forever
  until the user manually closes it or presses `^C`.  Instead, we should close
  the old terminal automatically if it's still open when the user starts a new
  synthesis operation.
- Currently, the user can only run synthesis on complete proofs, ending in
  either `Admitted.` or `Qed.`.  If the user is working incrementally (writing
  a lemma, synthesizing or writing a proof, and then writing the next lemma),
  it would be convenient to allow synthesizing a proof for the last lemma in
  the file without a trailing `Admitted.`.  In this case, the proof would end
  at EOF instead of at `Admitted.`/`Qed.`.
- Currently, the levels of the search tree are ordered left to right with
  constant spacing; if the tree is very deep, this makes the expanded tree much
  too wide to fit on one screen.  We should explore dynamic spacing or
  alternate arrangements to make it easier to navigate the expanded tree.
- The Python component doesn't currently provide much feedback when the search
  is ongoing.  It would be nice to show the user some kind of progress bar, or
  at least a counter, so that it's clear something is happening, even if it's
  not feasible to provide an ETA.
- Currently, the search tree shows only the tactics that were tried.  It would
  be useful to show the proof state when mousing over or clicking on each node
  so the user can see which branches of the search were actually making useful
  progress.
- In cases where the search produced something useful but fell short of a
  complete proof, it would be nice to provide a way for the user to paste in a
  partial proof from the search tree view.  For example, let the user click on
  a node in the tree to insert the sequence of tactics leading up to that node.
- The Python script currently prints a bunch of warnings from numpy and/or
  pytorch.  We should investigate and fix these warnings so that users don't
  mistakenly think that the script is broken.
