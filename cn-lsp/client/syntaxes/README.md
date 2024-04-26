# Development Notes

This syntax was created based on the tokens that CN lexes specially, which were
determined by looking at
[`c_lexer.mll`](https://github.com/rems-project/cerberus/blob/7441ffc7d32af2b33c8dca0699347314f87ece0f/parsers/c/c_lexer.mll).
Of note, some of those tokens appear not to be documented in the tutorial or
used in the examples it includes:
- `have`
- `pack`
- `set`
- `tuple`
- `unpack`

Since I don't know the semantics of these tokens, I haven't written highlighting
rules for them, so they'll remain unstyled.

[VSCode's
reference](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
is a useful starting point, and ongoing reference, for creating these grammars.

The naming conventions for tokens were based on [Sublime Text's reference
document](https://www.sublimetext.com/docs/scope_naming.html).

I found that `vscode-textmate` hosts a [broad
collection](https://github.com/microsoft/vscode-textmate/tree/09effd8b7429b71010e0fa34ea2e16e622692946/test-cases/first-mate/fixtures)
of grammars, which are useful as references when crafting rules.

I used [this
approach](https://stackoverflow.com/questions/46377151/how-to-customize-the-color-of-custom-syntax-tokens-in-vscode-extension/70504614)
to override the colors of particular token types, though I find it a little less
than satisfying. I would prefer to define the colors at the token definition
site in the grammar file, or at least somewhere that isn't `package.json`, which
feels too general, but I didn't see how to do that. 

