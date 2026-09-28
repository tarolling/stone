//! A language server for stone.
//!
//! [`run`] speaks the Language Server Protocol over a [`Connection`]. Each open document is
//! analyzed with [`stone::driver::analyze`] whenever it changes, which publishes its diagnostics,
//! and requests like hover and rename are answered from that analysis by [`features`].

use std::collections::HashMap;
use std::error::Error;

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

use crate::features::Document;
use crate::line_index::Encoding;

pub mod features;
pub mod line_index;

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
        documents: HashMap::new(),
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
    documents: HashMap<Uri, Document>,
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
                let location = features::definition(doc, &at.text_document.uri, at.position);
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
                    doc, position,
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

        // a closed document's diagnostics are cleared
        let diagnostics = match text {
            Some(text) => {
                let doc = Document::new(text, self.encoding);
                let diagnostics = features::diagnostics(&doc);
                self.documents.insert(uri.clone(), doc);
                diagnostics
            }
            None => {
                self.documents.remove(&uri);
                vec![]
            }
        };
        let params = PublishDiagnosticsParams::new(uri, diagnostics, None);
        vec![Notification::new(
            PublishDiagnostics::METHOD.to_string(),
            params,
        )]
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
