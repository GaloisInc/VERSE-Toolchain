import * as vsc from 'vscode';

import * as ct from 'vscode-languageclient/node';

let client: ct.LanguageClient;

export function activate(_context: vsc.ExtensionContext): void {
    // This is where we'll eventually configure and launch a client-server
    // relationship. For the moment, since this client only provides syntax
    // highlighting, this function needn't do anything.
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    } else {
        return client.stop();
    }
}
