// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{
    Action, BoxedWorkerFactory, OperationRecorder, OperationRecorderFactory, OperationStream,
    StateOperation,
};
use reflex_utils::{load_messages, FileWriter, FileWriterFormat};
use serde::{Deserialize, Serialize};
use std::{
    fs::File,
    path::{Path, PathBuf},
};

pub struct FileRecorder {
    format: FileWriterFormat,
    path: PathBuf,
}
impl FileRecorder {
    pub fn new(format: FileWriterFormat, path: impl Into<PathBuf>) -> Self {
        Self {
            format,
            path: path.into(),
        }
    }
    pub fn load<TAction: Action>(
        &self,
    ) -> Result<Vec<StateOperation<TAction>>, FileRecorderLoadError>
    where
        for<'de> StateOperation<TAction>: Deserialize<'de>,
    {
        let input_file = File::open(self.path.as_path()).map_err(FileRecorderLoadError::Load)?;
        load_messages(self.format, &input_file).map_err(FileRecorderLoadError::Deserialize)
    }
}

#[derive(Debug)]
pub enum FileRecorderLoadError {
    Load(std::io::Error),
    Deserialize(Box<dyn std::error::Error + Send + Sync>),
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

impl<TAction: Action + Clone + Serialize + Send + 'static>
    OperationRecorderFactory<FileWriterOperationRecorder<TAction>> for FileRecorder
{
    fn create(&self) -> FileWriterOperationRecorder<TAction> {
        // TODO: handle errors when creating session recorder
        let output_file = open_output_file(self.path.as_path()).unwrap();
        FileWriter::new(self.format, output_file).into()
    }
}

pub struct FileWriterOperationRecorder<TAction: Action + Clone + Serialize + Send + 'static>(
    FileWriter<StateOperation<TAction>>,
);
impl<TAction: Action + Clone + Serialize + Send + 'static> OperationRecorder
    for FileWriterOperationRecorder<TAction>
{
    type Action = TAction;
    fn record(&mut self, operation: &StateOperation<Self::Action>) {
        let Self(writer) = self;
        // TODO: handle errors when recording operations
        let _ = writer.write(match operation {
            StateOperation::Send(pid, action) => StateOperation::Send(*pid, action.clone()),
            StateOperation::Task(pid, _) => StateOperation::Task(*pid, OperationStream::noop()),
            StateOperation::Spawn(pid, _) => {
                StateOperation::Spawn(*pid, BoxedWorkerFactory::noop())
            }
            StateOperation::Kill(pid) => StateOperation::Kill(*pid),
        });
    }
}
impl<TAction: Action + Clone + Serialize + Send + 'static> From<FileWriter<StateOperation<TAction>>>
    for FileWriterOperationRecorder<TAction>
{
    fn from(writer: FileWriter<StateOperation<TAction>>) -> Self {
        Self(writer)
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
