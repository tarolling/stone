//! Drives the language server over an in-memory connection, the way an editor would.

use std::path::{Path, PathBuf};
use std::thread::JoinHandle;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use serde_json::{Value, json};

const URI: &str = "file:///demo.stn";

/// A running server and the client end of its connection.
struct Session {
    client: Connection,
    server: Option<JoinHandle<()>>,
    next_id: i32,
}

impl Session {
    /// Starts a server and completes the initialize handshake, offering `encodings` as the
    /// client's position encodings. Returns the session and the server's capabilities.
    fn start(encodings: Option<&[&str]>) -> (Session, Value) {
        let capabilities = match encodings {
            Some(encodings) => json!({ "general": { "positionEncodings": encodings } }),
            None => json!({}),
        };
        Session::start_with(capabilities)
    }

    /// Starts a server like [`Session::start`], for a client with these capabilities.
    fn start_with(capabilities: Value) -> (Session, Value) {
        let (server, client) = Connection::memory();
        let handle = std::thread::spawn(move || stone_lsp::run(server).unwrap());
        let mut session = Session {
            client,
            server: Some(handle),
            next_id: 0,
        };
        let result = session.request("initialize", json!({ "capabilities": capabilities }));
        session.notify("initialized", json!({}));
        (session, result["capabilities"].clone())
    }

    fn notify(&self, method: &str, params: Value) {
        let notification = Notification::new(method.to_string(), params);
        self.client.sender.send(notification.into()).unwrap();
    }

    fn send_request(&mut self, method: &str, params: Value) -> RequestId {
        self.next_id += 1;
        let id = RequestId::from(self.next_id);
        let request = Request::new(id.clone(), method.to_string(), params);
        self.client.sender.send(request.into()).unwrap();
        id
    }

    /// Returns the next message from the server, failing the test if none arrives in time.
    fn receive(&self) -> Message {
        self.client
            .receiver
            .recv_timeout(Duration::from_secs(5))
            .expect("server should respond")
    }

    fn response(&mut self, method: &str, params: Value) -> Response {
        let id = self.send_request(method, params);
        match self.receive() {
            Message::Response(response) => {
                assert_eq!(response.id, id);
                response
            }
            other => panic!("expected a response to {method}, got {other:?}"),
        }
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        match self.response(method, params).response_result {
            Ok(result) => result,
            Err(error) => panic!("{method} failed: {error:?}"),
        }
    }

    /// Returns the next diagnostics the server publishes.
    fn diagnostics(&self) -> Value {
        let (uri, diagnostics) = self.published();
        assert_eq!(uri, URI);
        diagnostics
    }

    /// Returns the next diagnostics the server publishes, and the URI of the file they are for.
    fn published(&self) -> (String, Value) {
        match self.receive() {
            Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => (
                n.params["uri"].as_str().unwrap().to_string(),
                n.params["diagnostics"].clone(),
            ),
            other => panic!("expected diagnostics, got {other:?}"),
        }
    }

    fn open(&self, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": { "uri": URI, "languageId": "stone", "version": 1, "text": text }
            }),
        );
    }

    /// Shuts the server down and checks that it exits cleanly.
    fn shut_down(mut self) {
        self.request("shutdown", Value::Null);
        self.notify("exit", Value::Null);
        self.server.take().unwrap().join().unwrap();
    }
}

fn position(line: u32, character: u32) -> Value {
    json!({ "textDocument": { "uri": URI }, "position": { "line": line, "character": character } })
}

#[test]
fn prefers_utf8_positions_when_offered() {
    let (session, capabilities) = Session::start(Some(&["utf-16", "utf-8"]));
    assert_eq!(capabilities["positionEncoding"], "utf-8");
    session.shut_down();
}

#[test]
fn asks_for_completion_after_a_dot() {
    let (session, capabilities) = Session::start(None);
    assert_eq!(
        capabilities["completionProvider"]["triggerCharacters"],
        json!(["."])
    );
    session.shut_down();
}

#[test]
fn falls_back_to_utf16_positions() {
    let (session, capabilities) = Session::start(None);
    assert_eq!(capabilities["positionEncoding"], "utf-16");
    assert_eq!(capabilities["hoverProvider"], true);
    assert_eq!(capabilities["renameProvider"]["prepareProvider"], true);
    assert_eq!(capabilities["codeActionProvider"], true);
    session.shut_down();
}

#[test]
fn publishes_diagnostics_as_the_document_changes() {
    let (session, _) = Session::start(None);

    session.open("x = 1\nx = \"s\"\nprint(x)\n");
    let diagnostics = session.diagnostics();
    assert_eq!(
        diagnostics,
        json!([{
            "range": {
                "start": { "line": 1, "character": 4 },
                "end": { "line": 1, "character": 7 }
            },
            "severity": 1,
            "source": "stone",
            "message": "cannot assign str to 'x', which is int",
            "relatedInformation": [{
                "location": {
                    "uri": URI,
                    "range": {
                        "start": { "line": 0, "character": 0 },
                        "end": { "line": 0, "character": 1 }
                    }
                },
                "message": "'x' is first assigned here"
            }]
        }])
    );

    session.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": URI, "version": 2 },
            "contentChanges": [{ "text": "x = 1\nx = 2\nprint(x)\n" }]
        }),
    );
    assert_eq!(session.diagnostics(), json!([]));

    session.notify(
        "textDocument/didClose",
        json!({ "textDocument": { "uri": URI } }),
    );
    assert_eq!(session.diagnostics(), json!([]));
    session.shut_down();
}

#[test]
fn answers_requests_about_an_open_document() {
    let (mut session, _) = Session::start(None);
    session.open("fn twice(n: int) -> int:\n    ret n * 2\nprint(twice(4))\n");
    session.diagnostics();

    let hover = session.request("textDocument/hover", position(2, 7));
    assert_eq!(
        hover["contents"]["value"],
        "```stone\nfn twice(n: int) -> int\n```"
    );

    let definition = session.request("textDocument/definition", position(2, 7));
    assert_eq!(
        definition["range"]["start"],
        json!({ "line": 0, "character": 3 })
    );

    let mut params = position(0, 5);
    params["context"] = json!({ "includeDeclaration": true });
    let references = session.request("textDocument/references", params);
    assert_eq!(references.as_array().unwrap().len(), 2);

    let mut params = position(1, 8);
    params["newName"] = json!("count");
    let edit = session.request("textDocument/rename", params);
    // `n` in `fn twice(n: int)` and in `ret n * 2`
    assert_eq!(edit["changes"][URI].as_array().unwrap().len(), 2);

    let symbols = session.request(
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": URI } }),
    );
    assert_eq!(symbols[0]["name"], "twice");

    let completion = session.request("textDocument/completion", position(2, 0));
    let labels: Vec<&str> = completion
        .as_array()
        .unwrap()
        .iter()
        .map(|item| item["label"].as_str().unwrap())
        .collect();
    assert!(labels.contains(&"twice") && labels.contains(&"print"));

    // nothing is at a blank position
    assert_eq!(
        session.request("textDocument/hover", position(4, 0)),
        Value::Null
    );
    session.shut_down();
}

#[test]
fn definition_of_a_builtin_opens_its_documentation() {
    let (mut session, _) = Session::start(None);
    session.open("print(1)\n");
    session.diagnostics();

    let location = session.request("textDocument/definition", position(0, 2));
    let uri = location["uri"].as_str().unwrap();
    assert!(
        uri.starts_with("file:///") && uri.ends_with("/builtins.stn"),
        "{uri}"
    );
    let path = stone_lsp::uri::file_path(&uri.parse().unwrap()).unwrap();
    let text = std::fs::read_to_string(path).unwrap();
    let line = location["range"]["start"]["line"].as_u64().unwrap() as usize;
    assert!(text.lines().nth(line).unwrap().starts_with("// print("));
    session.shut_down();
}

#[test]
fn the_prelude_a_definition_opens_has_no_errors() {
    let (mut session, _) = Session::start(None);
    session.open("x = some(1)\nprint(x)\n");
    session.diagnostics();

    let location = session.request("textDocument/definition", position(0, 5));
    let uri = location["uri"].as_str().unwrap().to_string();
    assert!(uri.ends_with("/prelude.stn"), "{uri}");
    let path = stone_lsp::uri::file_path(&uri.parse().unwrap()).unwrap();
    let text = std::fs::read_to_string(path).unwrap();
    session.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": { "uri": uri, "languageId": "stone", "version": 1, "text": text }
        }),
    );
    // its `none` variant is no error there, nor anything else
    let (published, diagnostics) = session.published();
    assert_eq!(published, uri);
    assert_eq!(diagnostics, json!([]));
    // then the other open document again
    session.diagnostics();
    session.shut_down();
}

#[test]
fn formatting_rewrites_the_document() {
    let (mut session, capabilities) = Session::start(None);
    assert_eq!(capabilities["documentFormattingProvider"], true);
    session.open("x=1\n");
    session.diagnostics();
    let edits = session.request(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": URI },
            "options": { "tabSize": 4, "insertSpaces": true }
        }),
    );
    assert_eq!(edits[0]["newText"], "x = 1\n");
    session.shut_down();
}

#[test]
fn rename_errors_are_reported_to_the_client() {
    let (mut session, _) = Session::start(None);
    session.open("x = 1\n");
    session.diagnostics();
    let mut params = position(0, 0);
    params["newName"] = json!("while");
    let response = session.response("textDocument/rename", params);
    assert_eq!(
        response.response_result.unwrap_err().message,
        "'while' is a keyword"
    );
    session.shut_down();
}

#[test]
fn unknown_requests_are_errors_and_the_server_keeps_going() {
    let (mut session, _) = Session::start(None);
    let response = session.response("textDocument/unknownThing", json!({}));
    assert_eq!(
        response.response_result.unwrap_err().code,
        lsp_server::ErrorCode::MethodNotFound as i32
    );
    // requests about documents that are not open have no answer
    assert_eq!(
        session.request("textDocument/hover", position(0, 0)),
        Value::Null
    );
    session.shut_down();
}

#[test]
fn a_change_to_one_file_republishes_the_others_in_its_program() {
    // the files only exist in the editor, so the server must read them from the open documents
    let main = "file:///stone-lsp-test-project/main.stn";
    let util = "file:///stone-lsp-test-project/util.stn";
    let (session, _) = Session::start(None);
    let open = |uri: &str, text: &str| {
        session.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": { "uri": uri, "languageId": "stone", "version": 1, "text": text }
            }),
        );
    };
    // the next diagnostics published for each uri, in the order they arrive
    let published = |count: usize| -> Vec<(String, usize)> {
        (0..count)
            .map(|_| match session.receive() {
                Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => (
                    n.params["uri"].as_str().unwrap().to_string(),
                    n.params["diagnostics"].as_array().unwrap().len(),
                ),
                other => panic!("expected diagnostics, got {other:?}"),
            })
            .collect()
    };

    open(util, "pub fn twice(x: int) -> int:\n    ret x * 2\n");
    assert_eq!(published(1), [(util.to_string(), 0)]);
    open(main, "use util\nprint(util.twice(1))\n");
    assert_eq!(published(2), [(main.to_string(), 0), (util.to_string(), 0)]);

    // making `twice` private breaks main.stn
    session.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": util, "version": 2 },
            "contentChanges": [{ "text": "fn twice(x: int) -> int:\n    ret x * 2\n" }]
        }),
    );
    assert_eq!(published(2), [(util.to_string(), 0), (main.to_string(), 1)]);
    session.shut_down();
}

/// Returns a new empty directory for a test named `name`.
fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("stone-lsp-test-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn uri_of(path: &Path) -> String {
    stone_lsp::uri::file_uri(path).unwrap().as_str().to_string()
}

#[test]
fn watches_manifests_and_publishes_their_errors() {
    let dir = scratch("manifests");
    let (app, geo) = (dir.join("app"), dir.join("geo"));
    std::fs::create_dir_all(&app).unwrap();
    std::fs::create_dir_all(&geo).unwrap();
    let main = "use geo\nprint(geo.one())\n";
    std::fs::write(app.join("main.stn"), main).unwrap();
    let manifest = "[dependencies]\ngeo = { path = \"../geo\" }\n";
    std::fs::write(app.join("stone.toml"), manifest).unwrap();
    std::fs::write(geo.join("lib.stn"), "pub fn one() -> int:\n    ret 1\n").unwrap();

    let (session, _) = Session::start_with(json!({
        "workspace": { "didChangeWatchedFiles": { "dynamicRegistration": true } }
    }));
    // the server asks to hear of changes to manifests and lockfiles
    let request = match session.receive() {
        Message::Request(request) => request,
        other => panic!("expected a request, got {other:?}"),
    };
    assert_eq!(request.method, "client/registerCapability");
    let registration = &request.params["registrations"][0];
    assert_eq!(registration["method"], "workspace/didChangeWatchedFiles");
    assert_eq!(
        registration["registerOptions"]["watchers"],
        json!([{ "globPattern": "**/stone.toml" }, { "globPattern": "**/stone.lock" }])
    );
    let reply = Response::new_ok(request.id, Value::Null);
    session.client.sender.send(reply.into()).unwrap();

    let main_uri = uri_of(&app.join("main.stn"));
    session.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": { "uri": main_uri, "languageId": "stone", "version": 1, "text": main }
        }),
    );
    // `geo` has no stone.toml yet, which the `use` and the program's manifest both say
    let (uri, found) = session.published();
    assert_eq!(uri, main_uri);
    assert_eq!(
        found[0]["message"],
        "'geo' cannot be used: '../geo' has no stone.toml, so it is not a package"
    );
    let manifest_uri = uri_of(&app.join("stone.toml"));
    let (uri, found) = session.published();
    assert_eq!(uri, manifest_uri);
    assert_eq!(
        found[0]["range"]["start"],
        json!({ "line": 1, "character": 0 })
    );

    std::fs::write(geo.join("stone.toml"), "[package]\nname = \"geo\"\n").unwrap();
    let geo_manifest = uri_of(&geo.join("stone.toml"));
    session.notify(
        "workspace/didChangeWatchedFiles",
        json!({ "changes": [{ "uri": geo_manifest, "type": 1 }] }),
    );
    assert_eq!(session.published(), (main_uri, json!([])));
    // both manifests, the fixed one cleared
    let mut rest = vec![session.published(), session.published()];
    rest.sort_by(|a, b| a.0.cmp(&b.0));
    let mut expected = vec![(manifest_uri, json!([])), (geo_manifest, json!([]))];
    expected.sort_by(|a, b| a.0.cmp(&b.0));
    assert_eq!(rest, expected);
    session.shut_down();
    let _ = std::fs::remove_dir_all(&dir);
}
