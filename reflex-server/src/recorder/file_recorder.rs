// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::{Action, SchedulerCommand, TaskFactory};
use reflex_recorder::session_recorder::{OperationRecorder, OperationRecorderFactory};
use reflex_utils::{load_messages, FileWriter, FileWriterFormat};
use serde::{Deserialize, Serialize};
use std::{
    fs::File,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug)]
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
    pub fn load<TAction, TTask>(
        &self,
    ) -> Result<Vec<SchedulerCommand<TAction, TTask>>, FileRecorderLoadError>
    where
        for<'de> SchedulerCommand<TAction, TTask>: Deserialize<'de>,
        TAction: Action,
        TTask: TaskFactory<TAction, TTask>,
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

impl<TAction, TTask> OperationRecorderFactory<FileWriterOperationRecorder<TAction, TTask>>
    for FileRecorder
where
    TAction: Action + Clone + Serialize + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Clone + Serialize + Send + 'static,
{
    fn create(&self) -> FileWriterOperationRecorder<TAction, TTask> {
        // TODO: handle errors when creating session recorder
        let output_file = open_output_file(self.path.as_path()).unwrap();
        FileWriter::new(self.format, output_file).into()
    }
}

pub struct FileWriterOperationRecorder<TAction, TTask>(
    FileWriter<SchedulerCommand<TAction, TTask>>,
)
where
    TAction: Action + Clone + Serialize + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static;
impl<TAction, TTask> OperationRecorder for FileWriterOperationRecorder<TAction, TTask>
where
    TAction: Action + Clone + Serialize + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Clone + Serialize + Send + 'static,
{
    type Action = TAction;
    type Task = TTask;
    fn record(&mut self, operation: &SchedulerCommand<Self::Action, Self::Task>) {
        let Self(writer) = self;
        // TODO: handle errors when recording operations
        let _ = writer.write(match operation {
            SchedulerCommand::Forward(pid) => SchedulerCommand::Forward(*pid),
            SchedulerCommand::Send(pid, action) => SchedulerCommand::Send(*pid, action.clone()),
            SchedulerCommand::Task(pid, task) => SchedulerCommand::Task(*pid, task.clone()),
            SchedulerCommand::Kill(pid) => SchedulerCommand::Kill(*pid),
        });
    }
}
impl<TAction, TTask> From<FileWriter<SchedulerCommand<TAction, TTask>>>
    for FileWriterOperationRecorder<TAction, TTask>
where
    TAction: Action + Clone + Serialize + Send + 'static,
    TTask: TaskFactory<TAction, TTask> + Send + 'static,
{
    fn from(writer: FileWriter<SchedulerCommand<TAction, TTask>>) -> Self {
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
