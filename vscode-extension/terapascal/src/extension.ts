import * as vscode from 'vscode'
import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node'

let client: LanguageClient
let output: vscode.OutputChannel

const CLIENT_ID = "terapascal-language-client"
const CLIENT_DESC = "Terapascal Language Client"

export async function activate(ctx: vscode.ExtensionContext) {
	output = vscode.window.createOutputChannel("Terapascal")
	ctx.subscriptions.push(output)

	info("starting...")
	
	let binPath = process.env["TERAPASCAL_BIN_PATH"]
	let command = binPath ? binPath + "/terapascal-ls" : "terapascal-ls"

	let run: Executable = {
		command,
		options: {
			env: {
				"RUST_LOG": "debug",
				...process.env
			},
		},
	}

	let serverOpts : ServerOptions = {
		run,
		debug: run,
	}

	let clientOpts: LanguageClientOptions = {
		documentSelector: [{
			scheme: "file",
			language: "terapascal",
		}],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
		},
		outputChannel: output,
	}

	client = new LanguageClient(CLIENT_ID, CLIENT_DESC, serverOpts, clientOpts)
	ctx.subscriptions.push(client)

	try {
		await client.start()
		info("client initialized")
	} catch (e) {
		error("client failed to start")
	}
}

export function deactivate() {
	if (client) {
		client.stop()
	} else {
		return undefined
	}
}

function info(msg: string) {
	output.appendLine("[client] " + msg)
}

function error(msg: string) {
	output.appendLine("[client] error: " + msg)
}