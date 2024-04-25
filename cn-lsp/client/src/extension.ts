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

    // A VSCode-interpretable identifier for this package (e.g. if we define a
    // setting "foo", you might see an entry in a user's `settings.json` keyed
    // by "cnClient.foo"). These tend to be camelCase.
    const clientID: string = 'cnClient';

    // A human-readable identifier for this package. I don't know the entirety
    // of how this information is used, but it at least appears in some error
    // messages displayed to the user, suffixed by " client".
    const clientName: string = 'CN';

    client = new ct.LanguageClient(clientID, clientName, serverOptions, clientOptions);
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
