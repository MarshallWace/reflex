// SPDX-FileCopyrightText: 2023 Marshall Wace <opensource@mwam.com>
// SPDX-License-Identifier: Apache-2.0
// SPDX-FileContributor: Tim Kendrick <t.kendrick@mwam.com> https://github.com/timkendrickmw
use std::error::Error;

use serde::{Deserialize, Serialize};
use tokio::{sync::mpsc, task::JoinHandle};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FileWriterFormat {
    Json,
    MessagePack,
}

trait WriteMessage {
    fn write(
        &self,
        message: &impl Serialize,
        output: &mut impl std::io::Write,
    ) -> Result<(), Box<dyn std::error::Error>>;
}

impl WriteMessage for FileWriterFormat {
    fn write(
        &self,
        message: &impl Serialize,
        output: &mut impl std::io::Write,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut output = output;
        match self {
            FileWriterFormat::Json => serde_json::to_writer(&mut output, message)
                .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
                .and_then(|_| {
                    write!(output, "\n").map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
                }),
            FileWriterFormat::MessagePack => rmp_serde::encode::write(output, message)
                .map_err(|err| Box::new(err) as Box<dyn std::error::Error>),
        }
    }
}

pub struct FileWriter<T: Send + 'static> {
    messages: mpsc::UnboundedSender<T>,
    task: JoinHandle<()>,
}

impl<T: Send + 'static> Drop for FileWriter<T> {
    fn drop(&mut self) {
        self.task.abort();
    }
}

impl<T> FileWriter<T>
where
    T: Send + Serialize + 'static,
{
    pub fn new(
        format: FileWriterFormat,
        output_file: impl std::io::Write + Send + 'static,
    ) -> Self {
        let (messages_tx, mut messages_rx) = mpsc::unbounded_channel();
        Self {
            messages: messages_tx,
            task: tokio::spawn({
                let mut output_file = output_file;
                let formatter = format;
                async move {
                    while let Some(message) = messages_rx.recv().await {
                        // TODO: handle errors when recording operations
                        let _ = formatter.write(&message, &mut output_file);
                    }
                }
            }),
        }
    }
    pub fn write(&self, message: T) -> Result<(), mpsc::error::SendError<T>> {
        self.messages.send(message)
    }
}

fn load_messages_rmp<T: for<'de> Deserialize<'de>>(
    input_file: impl std::io::Read,
) -> Result<Vec<T>, Box<dyn Error + Send + Sync>> {
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
                    return Err(Box::new(rmp_serde::decode::Error::InvalidMarkerRead(err)));
                }
            }
            Err(err) => {
                return Err(Box::new(err));
            }
        }
    }
    Ok(results)
}

fn load_messages_json<T: for<'de> Deserialize<'de>>(
    input_file: impl std::io::Read,
) -> Result<Vec<T>, Box<dyn Error + Send + Sync>> {
    let mut input_file = input_file;
    let mut input_as_string = String::new();
    let _ = input_file.read_to_string(&mut input_as_string)?;
    let results: Result<Vec<_>, _> = {
        let mut deserializer = serde_json::Deserializer::from_str(&input_as_string);
        deserializer.disable_recursion_limit();
        deserializer.into_iter::<T>().collect()
    };

    results.map_err(|err| Box::new(err) as Box<dyn std::error::Error + Sync + Send>)
}

pub fn load_messages<T: for<'de> Deserialize<'de>>(
    file_writer_format: FileWriterFormat,
    input_file: impl std::io::Read,
) -> Result<Vec<T>, Box<dyn Error + Send + Sync>> {
    match file_writer_format {
        FileWriterFormat::Json => load_messages_json(input_file),
        FileWriterFormat::MessagePack => load_messages_rmp(input_file),
    }
}

fn has_reached_eof(mut input_file: impl std::io::Read) -> bool {
    let mut foo = [];
    match input_file.read(&mut foo) {
        Ok(num_bytes_written) => num_bytes_written == 0,
        Err(_) => false,
    }
}
