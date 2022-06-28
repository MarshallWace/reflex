// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{OperationRecorder, OperationRecorderFactory};
use serde::{Deserialize, Serialize};
use std::{
    fs::File,
    path::{Path, PathBuf},
};
use tokio::{sync::mpsc, task::JoinHandle};

use reflex_dispatcher::{Action, ProcessId, StateOperation};

pub struct FileRecorder {
    path: PathBuf,
}
impl FileRecorder {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self { path: path.into() }
    }
    pub fn load<TAction: Action>(
        &self,
    ) -> Result<Vec<StateOperation<TAction>>, FileRecorderLoadError>
    where
        for<'de> StateOperation<TAction>: Deserialize<'de>,
    {
        let input_file = File::open(self.path.as_path()).map_err(FileRecorderLoadError::Load)?;
        load_messages(&input_file).map_err(FileRecorderLoadError::Deserialize)
    }
}

#[derive(Debug)]
pub enum FileRecorderLoadError {
    Load(std::io::Error),
    Deserialize(rmp_serde::decode::Error),
}
impl std::error::Error for FileRecorderLoadError {}
impl std::fmt::Display for FileRecorderLoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Load(err) => write!(f, "Failed to load session recording: {}", err),
            Self::Deserialize(err) => write!(f, "Failed to deserialize session recording: {}", err),
        }
    }
}

impl<TAction: Action + Serialize + Send + 'static>
    OperationRecorderFactory<FileRecorderWorker<TAction>> for FileRecorder
{
    fn create(&self) -> FileRecorderWorker<TAction> {
        // TODO: handle errors when creating session recorder
        let output_file = open_output_file(self.path.as_path()).unwrap();
        FileRecorderWorker::new(output_file)
    }
}

pub struct FileRecorderWorker<TAction: Action + Send + 'static> {
    messages: mpsc::UnboundedSender<StateOperation<TAction>>,
    task: JoinHandle<()>,
}
impl<TAction: Action + Send + 'static> Drop for FileRecorderWorker<TAction> {
    fn drop(&mut self) {
        self.task.abort();
    }
}
impl<TAction: Action + Send + 'static> OperationRecorder for FileRecorderWorker<TAction> {
    type Action = TAction;
    fn record(&mut self, operation: StateOperation<Self::Action>) {
        // TODO: handle errors when recording operations
        let _ = self.messages.send(operation);
    }
}
impl<TAction> FileRecorderWorker<TAction>
where
    TAction: Action + Send + Serialize + 'static,
{
    pub fn new(output_file: impl std::io::Write + Send + 'static) -> Self {
        let (messages_tx, mut messages_rx) = mpsc::unbounded_channel();
        Self {
            messages: messages_tx,
            task: tokio::spawn({
                let mut output_file = output_file;
                async move {
                    while let Some(message) = messages_rx.recv().await {
                        // TODO: handle errors when recording operations
                        let _ = write_message(
                            &SerializableStateOperation::from(&message),
                            &mut output_file,
                        );
                    }
                }
            }),
        }
    }
}

#[derive(Serialize, Debug)]
enum SerializableStateOperation<'a, TAction: Action + Serialize> {
    Send(ProcessId, &'a TAction),
    Task(ProcessId),
    Spawn(ProcessId),
    Kill(ProcessId),
}
impl<'a, TAction: Action + Serialize> From<&'a StateOperation<TAction>>
    for SerializableStateOperation<'a, TAction>
{
    fn from(operation: &'a StateOperation<TAction>) -> Self {
        // TODO: allow serializable tasks and spawned workers
        match operation {
            StateOperation::Send(pid, action) => Self::Send((*pid).into(), action),
            StateOperation::Task(pid, _) => Self::Task((*pid).into()),
            StateOperation::Spawn(pid, _) => Self::Spawn((*pid).into()),
            StateOperation::Kill(pid) => Self::Kill((*pid).into()),
        }
    }
}

fn open_output_file(path: &Path) -> Result<impl std::io::Write, String> {
    File::create(path).map_err(|err| {
        format!(
            "Failed to create session recorder output file: {} ({})",
            path.to_string_lossy(),
            err
        )
    })
}

fn write_message(
    message: &impl Serialize,
    output: &mut impl std::io::Write,
) -> Result<(), rmp_serde::encode::Error> {
    rmp_serde::encode::write(output, message)
}

fn load_messages<T: for<'de> Deserialize<'de>>(
    input_file: impl std::io::Read,
) -> Result<Vec<T>, rmp_serde::decode::Error> {
    let mut results = Vec::new();
    let mut input_file = input_file;
    loop {
        println!("Deserializing message {}..", results.len() + 1);
        match rmp_serde::from_read(&mut input_file) {
            Ok(result) => {
                results.push(result);
            }
            Err(rmp_serde::decode::Error::InvalidMarkerRead(err)) => {
                if has_reached_eof(&mut input_file) {
                    println!("Loaded {} messages", results.len());
                    break;
                } else {
                    return Err(rmp_serde::decode::Error::InvalidMarkerRead(err));
                }
            }
            Err(err) => {
                return Err(err);
            }
        }
    }
    Ok(results)
}

fn has_reached_eof(mut input_file: impl std::io::Read) -> bool {
    let mut foo = [];
    match input_file.read(&mut foo) {
        Ok(num_bytes_written) => num_bytes_written == 0,
        Err(_) => false,
    }
}
