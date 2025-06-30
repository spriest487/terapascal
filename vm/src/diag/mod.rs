use crate::heap::HeapStats;
use crate::stack::StackTrace;
use serde::Serialize;

#[cfg(not(feature = "debug_server"))]
pub use self::impl_null::*;

#[cfg(feature="debug_server")]
pub use self::impl_rouille::*;

#[derive(Debug, Clone, Serialize)]
pub struct DiagnosticOutput {
    pub stack_trace: StackTrace,
    pub heap_stats: HeapStats,
}

mod impl_null {
    use crate::diag::DiagnosticOutput;

    #[derive(Debug)]
    pub struct DiagnosticWorker;

    impl DiagnosticWorker {
        pub fn new(_port: u16) -> Option<Self> {
            None
        }

        pub fn update<DiagSource>(&self, _diag: DiagSource)
        where
            DiagSource: FnOnce() -> DiagnosticOutput,
        {
        }

        pub fn shutdown(self) {}
    }

    #[cfg(feature = "debug_server")]
    mod impl_rouille {
        use rouille::Response;
        use rouille::Server;
        use std::net::Ipv4Addr;
        use std::net::SocketAddr;
        use std::sync::mpsc::channel;
        use std::sync::mpsc::Receiver;
        use std::sync::mpsc::Sender;
        use std::sync::Arc;
        use std::sync::Mutex;
        use std::thread::JoinHandle;
        use super::DiagnosticOutput;

        #[derive(Debug)]
        pub struct DiagnosticWorker {
            join_handle: JoinHandle<()>,
            stop_server: Sender<()>,

            diag_send: Sender<DiagnosticOutput>,
            ready_recv: Receiver<()>,
        }

        impl DiagnosticWorker {
            pub fn new(port: u16) -> Option<Self> {
                let (ready_send, ready_recv) = channel::<()>();
                let (diag_send, diag_recv) = channel::<DiagnosticOutput>();

                let diag_recv = Arc::new(Mutex::new(diag_recv));

                let addr = SocketAddr::new(Ipv4Addr::LOCALHOST.into(), port);

                let server = Server::new(addr, move |_request| {
                    let ready_send = ready_send.clone();
                    if let Err(err) = ready_send.send(()) {
                        eprintln!("[diag] failed to read from signalling channel");
                        return Response::text(err.to_string()).with_status_code(500);
                    }

                    let next_diag = diag_recv.lock().unwrap().recv().unwrap();
                    Response::json(&next_diag)
                }).map_err(|err| {
                    eprintln!("[diag] failed to start server thread: {err}");
                }).ok()?;

                let (join_handle, stop_server) = server.stoppable();

                Some(Self {
                    join_handle,
                    stop_server,
                    diag_send,
                    ready_recv,
                })
            }

            pub fn update<DiagSource>(&self, diag: DiagSource)
            where
                DiagSource: FnOnce() -> DiagnosticOutput,
            {
                if self.ready_recv.try_recv().is_ok() {
                    let output = diag();

                    if !self.diag_send.send(output).is_ok() {
                        eprintln!("[diag] channel closed");
                    }
                }
            }

            pub fn shutdown(self) {
                if self.stop_server.send(()).is_ok() {
                    if let Err(..) = self.join_handle.join() {
                        eprintln!("[diag] failed to join diagnostics thread");
                    }
                }
            }
        }
    }
}
