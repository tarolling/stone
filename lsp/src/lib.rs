//! A language server for stone.
//!
//! [`run`] speaks the Language Server Protocol over a [`Connection`]. Each open document is
//! analyzed as part of the program it belongs to (see [`Document::in_program`]) whenever any open
//! document changes, which publishes its diagnostics, and requests like hover and rename are
//! answered from that analysis by [`features`].

use std::collections::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use lsp_server::{Connection, ErrorCode, ExtractError, Message, Notification, Request, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
    Notification as LspNotification, PublishDiagnostics,
};
use lsp_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, PrepareRenameRequest,
    References, Rename, Request as LspRequest,
};
use lsp_types::{
    CompletionOptions, CompletionResponse, DocumentSymbolResponse, GotoDefinitionResponse,
    HoverProviderCapability, InitializeParams, InitializeResult, OneOf, PositionEncodingKind,
    PublishDiagnosticsParams, RenameOptions, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri, WorkDoneProgressOptions,
};

use stone::project::{FsSources, Sources};

use crate::features::{Builtins, Document};
use crate::line_index::Encoding;
use crate::uri::{file_path, file_uri};

pub mod features;
pub mod line_index;
pub mod uri;

/// Runs the server until the client shuts it down.
///
/// The server prefers UTF-8 positions, which match stone's own columns more closely, and falls back
/// to UTF-16, the protocol's default, for clients that do not offer UTF-8.
pub fn run(connection: Connection) -> Result<(), Box<dyn Error + Send + Sync>> {
    let (id, params) = connection.initialize_start()?;
    let params: InitializeParams = serde_json::from_value(params)?;
    let encoding = negotiate_encoding(&params);
    let result = InitializeResult {
        capabilities: capabilities(encoding),
        server_info: Some(ServerInfo {
            name: "stone-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    };
    connection.initialize_finish(id, serde_json::to_value(result)?)?;

    let mut server = Server {
        encoding,
        texts: HashMap::new(),
        documents: HashMap::new(),
        builtins: write_builtins_reference().map(Builtins::new),
    };
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }
                connection
                    .sender
                    .send(server.handle_request(request).into())?;
            }
            Message::Notification(notification) => {
                for reply in server.handle_notification(notification) {
                    connection.sender.send(reply.into())?;
                }
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

/// Writes the builtins reference to a directory for this version of the server, returning its
/// URI, so that going to the definition of a builtin like `print` has a file to open.
///
/// For example, on Linux this writes `/tmp/stone-lsp-0.1.0/builtins.st`.
fn write_builtins_reference() -> Option<Uri> {
    let dir = std::env::temp_dir().join(format!("stone-lsp-{}", env!("CARGO_PKG_VERSION")));
    std::fs::create_dir_all(&dir).ok()?;
    let path = dir.join("builtins.st");
    // several servers can start at once, so replace the file whole rather than rewriting it
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    let unique = NEXT.fetch_add(1, Ordering::Relaxed);
    let temporary = dir.join(format!("builtins.st.{}.{unique}.tmp", std::process::id()));
    std::fs::write(&temporary, stone::stdlib::builtins_reference()).ok()?;
    std::fs::rename(&temporary, &path).ok()?;
    file_uri(&path)
}

/// Picks UTF-8 positions if the client supports them, and UTF-16 otherwise.
fn negotiate_encoding(params: &InitializeParams) -> Encoding {
    let offered = params
        .capabilities
        .general
        .as_ref()
        .and_then(|general| general.position_encodings.as_ref());
    match offered {
        Some(encodings) if encodings.contains(&PositionEncodingKind::UTF8) => Encoding::Utf8,
        _ => Encoding::Utf16,
    }
}

fn capabilities(encoding: Encoding) -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(match encoding {
            Encoding::Utf8 => PositionEncodingKind::UTF8,
            Encoding::Utf16 => PositionEncodingKind::UTF16,
        }),
        // documents are small, so the whole text on every change is simplest
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        document_symbol_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions::default()),
        ..ServerCapabilities::default()
    }
}

/// The server's state: the documents the client has open.
struct Server {
    encoding: Encoding,
    /// The text of each open document, which wins over what is on disk.
    texts: HashMap<Uri, String>,
    /// The analysis of each open document.
    documents: HashMap<Uri, Document>,
    /// Where definitions of builtins go, or `None` if the reference file could not be written.
    builtins: Option<Builtins>,
}

impl Server {
    /// Answers a request, with `null` for a document that is not open.
    fn handle_request(&self, request: Request) -> Response {
        match request.method.as_str() {
            HoverRequest::METHOD => self.answer::<HoverRequest>(request, |doc, params| {
                let position = params.text_document_position_params.position;
                Ok(features::hover(doc, position))
            }),
            GotoDefinition::METHOD => self.answer::<GotoDefinition>(request, |doc, params| {
                let at = params.text_document_position_params;
                let location = features::definition(
                    doc,
                    &at.text_document.uri,
                    at.position,
                    self.builtins.as_ref(),
                );
                Ok(location.map(GotoDefinitionResponse::Scalar))
            }),
            References::METHOD => self.answer::<References>(request, |doc, params| {
                let at = params.text_document_position;
                let include = params.context.include_declaration;
                Ok(Some(features::references(
                    doc,
                    &at.text_document.uri,
                    at.position,
                    include,
                )))
            }),
            PrepareRenameRequest::METHOD => self
                .answer::<PrepareRenameRequest>(request, |doc, params| {
                    Ok(features::prepare_rename(doc, params.position))
                }),
            Rename::METHOD => self.answer::<Rename>(request, |doc, params| {
                let at = params.text_document_position;
                features::rename(doc, &at.text_document.uri, at.position, &params.new_name)
                    .map(Some)
            }),
            DocumentSymbolRequest::METHOD => {
                self.answer::<DocumentSymbolRequest>(request, |doc, _| {
                    Ok(Some(DocumentSymbolResponse::Nested(
                        features::document_symbols(doc),
                    )))
                })
            }
            Completion::METHOD => self.answer::<Completion>(request, |doc, params| {
                let position = params.text_document_position.position;
                Ok(Some(CompletionResponse::Array(features::completion(
                    doc,
                    position,
                    &self.workspace(),
                ))))
            }),
            method => Response::new_err(
                request.id.clone(),
                ErrorCode::MethodNotFound as i32,
                format!("unknown request '{method}'"),
            ),
        }
    }

    /// Decodes the parameters of request `R`, finds its document, and encodes `answer`'s result,
    /// or its error message as a failed request.
    fn answer<R>(
        &self,
        request: Request,
        answer: impl FnOnce(&Document, R::Params) -> Result<R::Result, String>,
    ) -> Response
    where
        R: LspRequest,
        R::Params: DocumentParams,
        R::Result: Default,
    {
        let id = request.id.clone();
        let params = match request.extract::<R::Params>(R::METHOD) {
            Ok((_, params)) => params,
            Err(ExtractError::JsonError { error, .. }) => {
                return Response::new_err(id, ErrorCode::InvalidParams as i32, error.to_string());
            }
            Err(ExtractError::MethodMismatch(request)) => {
                unreachable!("dispatched {} as {}", request.method, R::METHOD)
            }
        };
        let Some(doc) = self.documents.get(params.uri()) else {
            return Response::new_ok(id, R::Result::default());
        };
        match answer(doc, params) {
            Ok(result) => Response::new_ok(id, result),
            Err(message) => Response::new_err(id, ErrorCode::RequestFailed as i32, message),
        }
    }

    /// Updates the open documents, returning the diagnostics to publish for any that changed.
    fn handle_notification(&mut self, notification: Notification) -> Vec<Notification> {
        let (uri, text) = match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let Ok(params) = notification
                    .extract::<<DidOpenTextDocument as LspNotification>::Params>(
                        DidOpenTextDocument::METHOD,
                    )
                else {
                    return vec![];
                };
                (params.text_document.uri, Some(params.text_document.text))
            }
            DidChangeTextDocument::METHOD => {
                let Ok(mut params) = notification
                    .extract::<<DidChangeTextDocument as LspNotification>::Params>(
                        DidChangeTextDocument::METHOD,
                    )
                else {
                    return vec![];
                };
                // with full sync, the last change holds the whole text
                let Some(change) = params.content_changes.pop() else {
                    return vec![];
                };
                (params.text_document.uri, Some(change.text))
            }
            DidCloseTextDocument::METHOD => {
                let Ok(params) = notification
                    .extract::<<DidCloseTextDocument as LspNotification>::Params>(
                        DidCloseTextDocument::METHOD,
                    )
                else {
                    return vec![];
                };
                (params.text_document.uri, None)
            }
            _ => return vec![],
        };

        match text {
            Some(text) => {
                self.texts.insert(uri.clone(), text);
            }
            None => {
                self.texts.remove(&uri);
                self.documents.remove(&uri);
            }
        }

        // a change to one file can change what is wrong in any other file of its program
        let workspace = self.workspace();
        self.documents = self
            .texts
            .iter()
            .map(|(uri, text)| {
                let doc = file_path(uri)
                    .and_then(|path| Document::in_program(&path, &workspace, self.encoding))
                    .unwrap_or_else(|| Document::new(text.clone(), self.encoding));
                (uri.clone(), doc)
            })
            .collect();

        // the changed document first, and a closed one's diagnostics are cleared
        let mut uris: Vec<&Uri> = self.documents.keys().filter(|u| **u != uri).collect();
        uris.sort_by_key(|u| u.as_str());
        let mut replies = vec![publish(&uri, self.documents.get(&uri))];
        replies.extend(uris.into_iter().map(|u| publish(u, self.documents.get(u))));
        replies
    }

    /// Returns the files of the workspace, with the open documents' text over what is on disk.
    fn workspace(&self) -> Workspace {
        Workspace {
            open: self
                .texts
                .iter()
                .filter_map(|(uri, text)| Some((file_path(uri)?, text.clone())))
                .collect(),
        }
    }
}

/// Returns the notification that publishes a document's diagnostics, or clears them if it is
/// closed.
fn publish(uri: &Uri, doc: Option<&Document>) -> Notification {
    let diagnostics = doc.map(features::diagnostics).unwrap_or_default();
    let params = PublishDiagnosticsParams::new(uri.clone(), diagnostics, None);
    Notification::new(PublishDiagnostics::METHOD.to_string(), params)
}

/// The files programs are read from: the open documents, then the filesystem.
struct Workspace {
    open: HashMap<PathBuf, String>,
}

impl Sources for Workspace {
    fn read(&self, path: &Path) -> Option<String> {
        self.open
            .get(path)
            .cloned()
            .or_else(|| FsSources.read(path))
    }

    fn is_dir(&self, path: &Path) -> bool {
        FsSources.is_dir(path) || self.open.keys().any(|p| p != path && p.starts_with(path))
    }

    fn entries(&self, dir: &Path) -> Vec<PathBuf> {
        let mut entries = FsSources.entries(dir);
        entries.extend(
            self.open
                .keys()
                .filter_map(|p| p.strip_prefix(dir).ok()?.components().next())
                .map(|first| dir.join(first)),
        );
        entries.sort();
        entries.dedup();
        entries
    }
}

/// Request parameters that name the document they are about.
trait DocumentParams {
    fn uri(&self) -> &Uri;
}

impl DocumentParams for lsp_types::HoverParams {
    fn uri(&self) -> &Uri {
        &self.text_document_position_params.text_document.uri
    }
}

impl DocumentParams for lsp_types::GotoDefinitionParams {
    fn uri(&self) -> &Uri {
        &self.text_document_position_params.text_document.uri
    }
}

impl DocumentParams for lsp_types::ReferenceParams {
    fn uri(&self) -> &Uri {
        &self.text_document_position.text_document.uri
    }
}

impl DocumentParams for lsp_types::TextDocumentPositionParams {
    fn uri(&self) -> &Uri {
        &self.text_document.uri
    }
}

impl DocumentParams for lsp_types::RenameParams {
    fn uri(&self) -> &Uri {
        &self.text_document_position.text_document.uri
    }
}

impl DocumentParams for lsp_types::DocumentSymbolParams {
    fn uri(&self) -> &Uri {
        &self.text_document.uri
    }
}

impl DocumentParams for lsp_types::CompletionParams {
    fn uri(&self) -> &Uri {
        &self.text_document_position.text_document.uri
    }
}
