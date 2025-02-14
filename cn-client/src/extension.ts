import * as vsc from "vscode";
import * as ct from "vscode-languageclient/node";

import child_process from "child_process";
import fs from "fs";
import path from "path";

let client: ct.LanguageClient;

type Maybe<T> = T | undefined;

export async function activate(context: vsc.ExtensionContext): Promise<void> {
    let serverContext = getConfiguredServerContext();

    if (serverContext === undefined) {
        serverContext = await newServerContext(context);
    }

    if (serverContext === undefined) {
        vsc.window.showErrorMessage("CN client: unable to find CN server");
        throw Error;
    }

    await setConfiguredServerContext(serverContext);

    let env = process.env;
    if (serverContext.cerbRuntime !== undefined) {
        env.CERB_RUNTIME = serverContext.cerbRuntime;
    } else {
        // TODO: we already tried to get ahold of the runtime when we generated
        // or retrieved in a server configuration, should we try again?
    }

    const serverOptions: ct.Executable = {
        command: serverContext.serverPath,
        transport: ct.TransportKind.pipe,
        options: {
            env,
        },
    };

    const clientOptions: ct.LanguageClientOptions = {
        // Send the server messages about C files
        documentSelector: [{ scheme: "file", language: "c" }],
    };

    // I'm not sure how this value's semantics differs from that of
    // `clientName`, below, but I think it's intended to be a single word,
    // perhaps with hyphens/underscores, and should match the "namespace" we use
    // for this client's contributions. For example, if we contribute a property
    // "foo", we should define it (in package.json) as "CN.foo".
    const clientID: string = "CN";

    // A human-readable identifier for this package. I don't know the entirety
    // of how this information is used, but it at least appears in some error
    // messages displayed to the user, suffixed by " client".
    const clientName: string = "CN";

    client = new ct.LanguageClient(
        clientID,
        clientName,
        serverOptions,
        clientOptions
    );

    vsc.commands.registerCommand("CN.runOnFile", () => {
        runCN();
    });

    vsc.commands.registerCommand(
        "CN.runOnFunction",
        (functionName: string, functionRange: ct.Range) => {
            runCN(functionName, functionRange);
        }
    );

    client.start();
    console.log("started client");
}

async function runCN(functionName?: string, functionRange?: ct.Range) {
    const req = new ct.RequestType("$/runCN");

    const activeEditor = vsc.window.activeTextEditor;
    if (activeEditor === undefined) {
        vsc.window.showErrorMessage("CN client: no file currently open");
        return;
    }

    const params: VerifyParams = {
        uri: activeEditor.document.uri.toString(),
        fn: functionName,
        fnRange: functionRange
    };

    client.sendRequest(req, params);
}

// This schema is meant to match the one defined by `cn-lsp`'s
// `Server.VerifyParams.t` type.
type VerifyParams = {
    uri: ct.DocumentUri;
    fn?: string;
    fnRange?: ct.Range;
};

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    } else {
        return client.stop();
    }
}

interface ServerContext {
    serverPath: string;
    cerbRuntime?: string;
}

async function newServerContext(
    context: vsc.ExtensionContext
): Promise<Maybe<ServerContext>> {
    let switches = child_process.spawnSync("opam", ["switch", "list", "-s"], {
        encoding: "utf-8",
    });
    if (switches.status === 0) {
        let switchNames = switches.stdout.trim().split("\n");
        let switchName = await vsc.window.showQuickPick(switchNames, {
            placeHolder:
                "Which opam switch contains your language server installation?",
        });
        if (switchName !== undefined) {
            // Whether the switch is local or global determines how we find its
            // opam directory
            let opamDir: string;
            if (path.isAbsolute(switchName)) {
                // The switch is local
                opamDir = path.join(switchName, "_opam");
            } else {
                opamDir = path.join("~", ".opam", switchName);
            }

            let serverPath = path.join(opamDir, "bin", "cn-lsp-server");
            let cerbRuntime = path.join(opamDir, "lib", "cerberus", "runtime");

            if (fs.existsSync(serverPath) && fs.existsSync(cerbRuntime)) {
                return {
                    serverPath,
                    cerbRuntime,
                };
            }
        }
    }

    console.log("Looking in $CN_LSP_SERVER");
    console.log(process.env);
    let envPath = process.env.CN_LSP_SERVER;
    if (envPath !== undefined) {
        return { serverPath: envPath };
    }

    console.log("Looking on $PATH");
    const out = child_process.spawnSync("which", ["cn-lsp-server"], {
        encoding: "utf-8",
    });
    if (out.status === 0) {
        let serverPath = out.stdout.trim();
        return { serverPath };
    }

    return undefined;
}

function getConfiguredServerContext(): Maybe<ServerContext> {
    let conf = vsc.workspace.getConfiguration("CN");
    let serverPath: Maybe<string> = conf.get("serverPath");
    let cerbRuntime: Maybe<string> = conf.get("cerbRuntime");

    // In practice, despite the type annotations, `conf.get` seems capable of
    // returning `null` values, so we need to check them
    if (
        serverPath !== null &&
        serverPath !== undefined &&
        serverPath !== "" &&
        cerbRuntime !== null &&
        cerbRuntime !== undefined &&
        cerbRuntime !== ""
    ) {
        return { serverPath, cerbRuntime };
    } else {
        return undefined;
    }
}

async function setConfiguredServerContext(serverContext: ServerContext) {
    let conf = vsc.workspace.getConfiguration("CN");
    await conf.update(
        "cerbRuntime",
        serverContext.cerbRuntime,
        vsc.ConfigurationTarget.Global
    );
    await conf.update(
        "serverPath",
        serverContext.serverPath,
        vsc.ConfigurationTarget.Global
    );
}
