//! Drives the language server over an in-memory connection, the way an editor would.

use std::thread::JoinHandle;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use serde_json::{Value, json};

const URI: &str = "file:///demo.st";

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
        let (server, client) = Connection::memory();
        let handle = std::thread::spawn(move || stone_lsp::run(server).unwrap());
        let mut session = Session {
            client,
            server: Some(handle),
            next_id: 0,
        };
        let capabilities = match encodings {
            Some(encodings) => json!({ "general": { "positionEncodings": encodings } }),
            None => json!({}),
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
        match self.receive() {
            Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => {
                assert_eq!(n.params["uri"], URI);
                n.params["diagnostics"].clone()
            }
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
fn falls_back_to_utf16_positions() {
    let (session, capabilities) = Session::start(None);
    assert_eq!(capabilities["positionEncoding"], "utf-16");
    assert_eq!(capabilities["hoverProvider"], true);
    assert_eq!(capabilities["renameProvider"]["prepareProvider"], true);
    session.shut_down();
}

#[test]
fn publishes_diagnostics_as_the_document_changes() {
    let (session, _) = Session::start(None);

    session.open("x = 1\nx = \"s\"\n");
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
            "message": "cannot assign str to 'x', which is int"
        }])
    );

    session.notify(
        "textDocument/didChange",
        json!({
            "textDocument": { "uri": URI, "version": 2 },
            "contentChanges": [{ "text": "x = 1\nx = 2\n" }]
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
    session.open("def twice(n);\n    ret n * 2\nprint(twice(4))\n");
    session.diagnostics();

    let hover = session.request("textDocument/hover", position(2, 7));
    assert_eq!(
        hover["contents"]["value"],
        "```stone\ndef twice(n: int) -> int\n```"
    );

    let definition = session.request("textDocument/definition", position(2, 7));
    assert_eq!(
        definition["range"]["start"],
        json!({ "line": 0, "character": 4 })
    );

    let mut params = position(0, 5);
    params["context"] = json!({ "includeDeclaration": true });
    let references = session.request("textDocument/references", params);
    assert_eq!(references.as_array().unwrap().len(), 2);

    let mut params = position(1, 8);
    params["newName"] = json!("count");
    let edit = session.request("textDocument/rename", params);
    // `n` in `def twice(n)` and in `ret n * 2`
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
        uri.starts_with("file:///") && uri.ends_with("/builtins.st"),
        "{uri}"
    );
    let text = std::fs::read_to_string(uri.trim_start_matches("file://")).unwrap();
    let line = location["range"]["start"]["line"].as_u64().unwrap() as usize;
    assert!(text.lines().nth(line).unwrap().starts_with("# print("));
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
