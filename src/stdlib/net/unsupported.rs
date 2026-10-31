//! The `net` module where stone has no sockets yet: Windows, where a program that uses the
//! module is an error before it runs (`driver::unsupported`). Nothing makes a [`Socket`] or a
//! [`Listener`], so their methods never run, and every function fails.

use crate::stdlib::fs::message;

/// What every function of the module fails with.
const UNSUPPORTED: &str = "the net module is not supported on this system yet";

/// A connection, which nothing makes here.
#[derive(Debug)]
pub enum Socket {}

impl Socket {
    pub fn set_timeout(&mut self, _: f64) {
        match *self {}
    }

    pub fn send(&mut self, _: &str) -> Result<i64, String> {
        match *self {}
    }

    pub fn send_bytes(&mut self, _: &[u8]) -> Result<i64, String> {
        match *self {}
    }

    pub fn read_line(&mut self) -> Result<Option<String>, String> {
        match *self {}
    }

    pub fn read(&mut self) -> Result<String, String> {
        match *self {}
    }

    pub fn read_all(&mut self) -> Result<String, String> {
        match *self {}
    }

    pub fn read_bytes(&mut self, _: i64) -> Result<Vec<u8>, String> {
        match *self {}
    }

    pub fn read_exact(&mut self, _: i64) -> Result<Vec<u8>, String> {
        match *self {}
    }

    pub fn read_all_bytes(&mut self) -> Result<Vec<u8>, String> {
        match *self {}
    }

    pub fn close(&mut self) {
        match *self {}
    }

    pub fn take_fd(&mut self, _: i64) -> Result<Vec<u8>, String> {
        match *self {}
    }

    pub fn shm_create(&mut self, _: i64) -> Result<i64, String> {
        match *self {}
    }

    pub fn shm_write(&mut self, _: i64, _: &[u8]) -> Result<i64, String> {
        match *self {}
    }

    pub fn shm_send(&mut self, _: &[u8]) -> Result<i64, String> {
        match *self {}
    }

    pub fn ready(&mut self, _: i64) -> bool {
        match *self {}
    }
}

/// A port waiting for connections, which nothing makes here.
#[derive(Debug)]
pub enum Listener {}

impl Listener {
    pub fn accept(&mut self) -> Result<Socket, String> {
        match *self {}
    }

    pub fn set_timeout(&mut self, _: f64) {
        match *self {}
    }

    pub fn port(&self) -> i64 {
        match *self {}
    }

    pub fn close(&mut self) {
        match *self {}
    }
}

/// Fails, since there are no sockets to connect.
pub fn connect(_: &[i64], _: i64, name: &str, _: i64) -> Result<Socket, String> {
    Err(message(UNSUPPORTED, name))
}

/// Fails, since there are no sockets to connect.
pub fn connect_unix(_: &str, name: &str) -> Result<Socket, String> {
    Err(message(UNSUPPORTED, name))
}

/// Fails, since there are no sockets to listen on.
pub fn listen(_: &[i64], _: i64, name: &str) -> Result<Listener, String> {
    Err(message(UNSUPPORTED, name))
}

/// Fails, since there are no sockets to ask through.
pub fn exchange(_: &[i64], _: i64, name: &str, _: &[i64], _: i64) -> Result<Vec<i64>, String> {
    Err(message(UNSUPPORTED, name))
}
