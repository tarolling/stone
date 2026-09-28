//! Runs the `stone-lsp` executable and talks to it over stdio, as an editor does.

use std::io::BufReader;
use std::process::{Command, Stdio};

use lsp_server::{Message, Notification, Request, RequestId};
use serde_json::json;

#[test]
fn serves_diagnostics_over_stdio_and_exits_cleanly() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_stone-lsp"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("stone-lsp should start");
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut send = |message: Message| message.write(&mut stdin).unwrap();
    let mut receive = || {
        Message::read(&mut stdout)
            .unwrap()
            .expect("server should reply")
    };

    send(
        Request::new(
            RequestId::from(1),
            "initialize".to_string(),
            json!({ "capabilities": {} }),
        )
        .into(),
    );
    let Message::Response(initialized) = receive() else {
        panic!("expected the initialize response");
    };
    assert_eq!(
        initialized.response_result.unwrap()["serverInfo"]["name"],
        "stone-lsp"
    );
    send(Notification::new("initialized".to_string(), json!({})).into());

    send(
        Notification::new(
            "textDocument/didOpen".to_string(),
            json!({
                "textDocument": {
                    "uri": "file:///bad.st",
                    "languageId": "stone",
                    "version": 1,
                    "text": "if 1\n    x = 1\n"
                }
            }),
        )
        .into(),
    );
    let Message::Notification(published) = receive() else {
        panic!("expected diagnostics");
    };
    assert_eq!(
        published.params["diagnostics"][0]["message"],
        "expected ';', found end of line"
    );

    send(Request::new(RequestId::from(2), "shutdown".to_string(), json!(null)).into());
    assert!(matches!(receive(), Message::Response(_)));
    send(Notification::new("exit".to_string(), json!(null)).into());
    drop(stdin);
    assert!(child.wait().unwrap().success());
}
