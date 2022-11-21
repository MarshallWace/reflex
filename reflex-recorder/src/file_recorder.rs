// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use reflex_dispatcher::Action;
use reflex_utils::{load_messages, FileWriter, FileWriterFormat};
use serde::{Deserialize, Serialize};
use std::{fs::File, path::Path};

use crate::session_recorder::ActionRecorder;

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

pub struct FileRecorder<TAction>(FileWriter<TAction>)
where
    TAction: Send + 'static;
impl<TAction> FileRecorder<TAction>
where
    TAction: Send + 'static,
{
    pub fn create(format: FileWriterFormat, path: &Path) -> Result<Self, String>
    where
        TAction: Clone + Serialize,
    {
        File::create(path)
            .map_err(|err| {
                format!(
                    "Failed to create session recorder output file: {} ({})",
                    path.to_string_lossy(),
                    err
                )
            })
            .map(|output_file| Self::from(FileWriter::new(format, output_file)))
    }
    pub fn load(
        format: FileWriterFormat,
        path: &Path,
    ) -> Result<Vec<TAction>, FileRecorderLoadError>
    where
        TAction: for<'de> Deserialize<'de>,
    {
        let input_file = File::open(path).map_err(FileRecorderLoadError::Load)?;
        load_messages(format, &input_file).map_err(FileRecorderLoadError::Deserialize)
    }
}
impl<TAction> ActionRecorder for FileRecorder<TAction>
where
    TAction: Action + Clone + Serialize + Send + 'static,
{
    type Action = TAction;
    fn record(&mut self, action: &Self::Action) {
        let Self(writer) = self;
        // TODO: handle errors when recording operations
        let _ = writer.write(action.clone());
    }
}
impl<TAction> From<FileWriter<TAction>> for FileRecorder<TAction>
where
    TAction: Clone + Serialize + Send + 'static,
{
    fn from(writer: FileWriter<TAction>) -> Self {
        Self(writer)
    }
}
