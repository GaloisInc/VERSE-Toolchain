// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import path from 'path';
//import fs from 'fs';
import fsPromises from 'fs/promises';
import crypto from 'crypto';
import * as vscode from 'vscode';

// Run a process in a new terminal and wait for it to report an exit code.
// This can happen by the process exiting completely (in which case the
// terminal also closes automatically), or by the process reporting an exit
// code through the shell integration API (in which case the terminal might
// remain open, e.g. to show an error message).
function runTerminalAsync(opts: vscode.TerminalOptions): Promise<number> {
    // TODO: close old terminal (if it still exists) before creating a new one
    const term = vscode.window.createTerminal(opts);
    term.show();
    return new Promise((resolve, reject) => {
        const token = vscode.window.onDidCloseTerminal((closedTerm) => {
            if (closedTerm === term) {
                token.dispose();
                token2.dispose();
                if (term.exitStatus == null || term.exitStatus.code == null) {
                    reject('terminal closed with no exit status');
                    return;
                }
                resolve(term.exitStatus.code);
            }
        });
        const token2 = vscode.window.onDidEndTerminalShellExecution((ev) => {
            if (ev.terminal === term && ev.exitCode != null) {
                token.dispose();
                token2.dispose();
                resolve(ev.exitCode);
            }
        });
    });
}


// `SingletonWebViewPanel` is based on `CatCodingPanel` from `webview-sample`
// in https://github.com/microsoft/vscode-extension-samples, used under the
// following license:
//
// MIT License
//
// Copyright (c) 2015 - present Microsoft Corporation
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Helper for managing the webview panel.  At most one such panel can exist at
// a time.
//
class SingletonWebViewPanel {
    /**
     * Track the currently panel. Only allow a single panel to exist at a time.
     */
    public static currentPanel: SingletonWebViewPanel | undefined;

    public static readonly viewType = 'coqSynthesis';

    private readonly _panel: vscode.WebviewPanel;
    private readonly _extensionUri: vscode.Uri;
    private _disposables: vscode.Disposable[] = [];

    private static getWebviewOptions(extensionUri: vscode.Uri): vscode.WebviewOptions {
        return {
            // Enable javascript in the webview
            enableScripts: true,

            // And restrict the webview to only loading content from our
            // extension's `webview` directory.
            localResourceRoots: [vscode.Uri.joinPath(extensionUri, 'webview')]
        };
    }

    public static createOrShow(extensionUri: vscode.Uri): SingletonWebViewPanel {
        let column = vscode.window.activeTextEditor?.viewColumn;
        if (column != undefined) {
            column += 1;
        } else {
            column = vscode.ViewColumn.One;
        }
        //const column = vscode.ViewColumn.Beside;

        // If we already have a panel, show it.
        if (SingletonWebViewPanel.currentPanel) {
            SingletonWebViewPanel.currentPanel._panel.reveal(column);
            return SingletonWebViewPanel.currentPanel;
        }

        // Otherwise, create a new panel.
        const panel = vscode.window.createWebviewPanel(
            SingletonWebViewPanel.viewType,
            'Coq Synthesis',
            column || vscode.ViewColumn.One,
            SingletonWebViewPanel.getWebviewOptions(extensionUri),
        );

        SingletonWebViewPanel.currentPanel = new SingletonWebViewPanel(panel, extensionUri);

        return SingletonWebViewPanel.currentPanel;
    }

    private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
        this._panel = panel;
        this._extensionUri = extensionUri;

        // Listen for when the panel is disposed
        // This happens when the user closes the panel or when the panel is closed programmatically
        this._panel.onDidDispose(() => this.dispose(), null, this._disposables);
    }

    public postMessage(msg: any) {
        this._panel.webview.postMessage(msg);
    }

    public dispose() {
        SingletonWebViewPanel.currentPanel = undefined;

        // Clean up our resources
        this._panel.dispose();

        while (this._disposables.length) {
            const x = this._disposables.pop();
            if (x) {
                x.dispose();
            }
        }
    }

    public setHtml(html: string) {
        this._panel.title = 'Coq Synthesis';
        this._panel.webview.html = html;
    }

    public getResourceUri(fileName: string): vscode.Uri {
        const localUri = vscode.Uri.joinPath(this._extensionUri, 'webview', fileName)
        return this._panel.webview.asWebviewUri(localUri);
    }
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
    const disposable = vscode.commands.registerCommand('coq-synthesis-vscode.synthesizeProof', async () => {
        // List of cleanup operations.  Functions from this list are run in
        // reverse order in the `finally` block.
        const cleanup = [];
        try {
            const config = vscode.workspace.getConfiguration('coq-synthesis-vscode');
            const proverbotDir: string | undefined = config.get('proverbot9001.path');
            if (proverbotDir == null || proverbotDir == '') {
                vscode.window.showErrorMessage(
                    'The `proverbot9001.path` setting must be set before using this command.');
                return;
            }
            let pythonExe = config.get('proverbot9001.pythonInterpreter');
            if (pythonExe == '') {
                pythonExe = path.join(proverbotDir, 'venv/bin/python3');
            }

            let extraEnv: {
                OPAMSWITCH?: string
            } = {};
            let opamSwitch: string | undefined = config.get('proverbot9001.opamSwitch');
            if (opamSwitch != null && opamSwitch != '') {
                extraEnv['OPAMSWITCH'] = opamSwitch;
            }

            const textEditor = vscode.window.activeTextEditor;
            if (textEditor == null) {
                return;
            }

            const document = textEditor.document;

            // Make the buffer read-only.  The proof search returns a range of
            // characters to overwrite with the new proof, and those positions will
            // be invalidated if the user modifies the file while the search is
            // running.
            console.log('make readonly');
            // TODO: If the buffer is already readonly, we should skip these
            // steps.  However, the read-only status doesn't seem to be exposed
            // in the extension API.
            vscode.commands.executeCommand(
                'workbench.action.files.setActiveEditorReadonlyInSession');
            cleanup.push(async () => {
                // Note that we may make the buffer writable below in order to
                // insert the synthesized proof.  In that case, this cleanup
                // will be a no-op.
                console.log('cleanup: make writeable');
                // Restore editor focus.  See below for why we need this.
                await vscode.window.showTextDocument(document, textEditor.viewColumn);
                vscode.commands.executeCommand(
                    'workbench.action.files.setActiveEditorWriteableInSession');
            });

            const filePath = document.uri.fsPath;
            const parentDir = path.dirname(filePath);
            const proofLine = textEditor.selection.active.line;


            // Write buffer content to a temp file.  This lets proverbot see the
            // current content even if the file hasn't been saved recently.
            const filenameBase = path.basename(filePath, '.v')
            const tempFileRandomness = crypto.randomBytes(16).toString('base64')
                .replaceAll('/', '_').replaceAll('+', '\'').replaceAll('=', '');
            const tempModuleName = `${filenameBase}__vscode_${tempFileRandomness}`;
            const tempFileName = tempModuleName + '.v';
            const tempFilePath = path.join(parentDir, tempFileName);
            await fsPromises.writeFile(tempFilePath, document.getText(), {
                'mode': 0o600,
                'flag': 'wx',
            });
            cleanup.push(async () => {
                console.log('cleanup: remove ' + tempFilePath);
                await fsPromises.unlink(tempFilePath);
            });
            console.log('temp file = ', tempFilePath);

            console.log('starting..');
            const wrapperScript = path.join(context.extensionPath, 'scripts', 'wait_on_error.sh');
            const exitCode = await runTerminalAsync({
                'name': 'Coq proof synthesis',
                'message': 'Running proof search...\r\n',
                'shellPath': wrapperScript,
                'shellArgs': [
                    proverbotDir + '/venv/bin/python3',
                    proverbotDir + '/src/search_file.py',
                    '--weightsfile', proverbotDir + '/data/polyarg-weights.dat',
                    tempFilePath,
                    '--proof-line', (1 + proofLine).toString(),
                    '--no-generate-report',
                    '--no-resume',
                ],
                'cwd': parentDir,
                'env': extraEnv,
            });
            console.log('done', exitCode);
            if (exitCode != 0) {
                vscode.window.showErrorMessage(
                    'Proof synthesis failed: exit code = ' + exitCode);
                return;
            }

            const resultPath = path.join(parentDir, 'search-report', tempModuleName + '-proofs.txt');
            cleanup.push(async () => {
                console.log('cleanup: remove ' + resultPath);
                //await fsPromises.unlink(resultPath);
            });
            const resultText = await fsPromises.readFile(resultPath, {'encoding': 'utf8'});
            const result = JSON.parse(resultText);
            const [resultDesc, resultProof, resultInfo] = result;


            // Display proof tree.  We show this regardless of the synthesis
            // result.
            const treeResultFileName =
                resultInfo['module_prefix'] + resultInfo['lemma_name'] + '.graph.json';
            const treeResultPath = path.join(parentDir, 'search-report', treeResultFileName);
            cleanup.push(async () => {
                console.log('cleanup: remove ' + treeResultPath);
                await fsPromises.unlink(treeResultPath);
            });
            const treeResultText = await fsPromises.readFile(treeResultPath, {'encoding': 'utf8'});
            console.log(treeResultText);
            const treeResult = JSON.parse(treeResultText);

            const panel = SingletonWebViewPanel.createOrShow(context.extensionUri);
            const treeJs = panel.getResourceUri('tree.js');
            const treeCss = panel.getResourceUri('tree.css');
            // TODO: vendor jquery and d3 instead of relying on third-party cdns
            panel.setHtml(`<!DOCTYPE html>
                <html>
                <head>
                <script src="http://code.jquery.com/jquery-1.10.2.min.js"></script>
                <script src="http://d3js.org/d3.v3.min.js"></script>
                <script src="${treeJs}"></script>
                <link href="${treeCss}" rel="stylesheet" />
                <style>
                body {
                    margin: 0;
                    padding: 0;
                    overflow: hidden;
                }
                </style>
                <script>
                window.addEventListener('message', (event) => {
                    renderTree(event.data);
                }, false);
                </script>
                </head>
                <body>
                <div id="tree-container"></div>
                </body>
                </html>
            `);
            panel.postMessage(treeResult);


            // Paste the synthesized proof into the buffer, replacing the
            // current proof.  This happens only if the synthesis succeeded.
            if (resultProof['status'] != 'SUCCESS') {
                vscode.window.showErrorMessage(
                    'Proof synthesis failed: status = ' + resultProof['status']);
                return;
            }

            console.log(' === proof ===');
            for (let cmd of resultProof['commands']) {
                console.log(cmd['tactic']);
            }
            const span = resultInfo['span'];
            const start = document.positionAt(span[1]);
            const end = document.positionAt(span[2]);
            const spanRange = new vscode.Range(start, end);

            console.log('make read-write');
            // Restore focus to the editor so we can make it writeable.
            // Passing the same `viewColumn` here causes vscode to focus the
            // existing editor in that column, instead of creating a new one in
            // the currently focused colum (which may be the side column where
            // the proof tree is now displayed).
            await vscode.window.showTextDocument(document, textEditor.viewColumn);
            vscode.commands.executeCommand(
                'workbench.action.files.setActiveEditorWriteableInSession');

            console.log('applying edit');
            textEditor.edit((editBuilder) => {
                let s = '';
                for (let cmd of resultProof['commands']) {
                    if (s != '') {
                        s += '\n';
                    }
                    s += cmd['tactic'];
                }
                console.log('new text = ', s);
                editBuilder.replace(spanRange, s);
            });

            console.log('all done');

        } finally {
            while (cleanup.length > 0) {
                const f = cleanup.pop();
                if (f != null) {
                    await f();
                }
            }
        }
    });

    context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated
export function deactivate() {}
