import * as vsc from 'vscode';

import * as ct from 'vscode-languageclient/node';

let client: ct.LanguageClient;

export function activate(context: vsc.ExtensionContext): void {
    // For the moment, don't refer to a particular server - we should still be
    // able to do some client-side development without one
    const serverPath = context.asAbsolutePath("nonexistent");
    const serverOptions: ct.Executable = {
        command: serverPath,
        transport: ct.TransportKind.stdio,
    };

    const clientOptions: ct.LanguageClientOptions = {};

    client = new ct.LanguageClient('CN', 'cn', serverOptions, clientOptions);
    client.start();
    console.log("started client");
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    } else {
        return client.stop();
    }
}
