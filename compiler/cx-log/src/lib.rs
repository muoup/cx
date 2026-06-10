use std::fmt::Formatter;
use std::path::{Path, PathBuf};

pub trait CXErrorTrait {
    fn pretty_print(&self);

    /// Attempt to downcast this error to a concrete type.
    /// Returns Some if the error is of the given type, None otherwise.
    fn as_any(&self) -> &dyn std::any::Any {
        &()
    }

    fn error_prefix(&self) -> String;

    fn error_content(&self) -> String;

    /// Get the error as a string for LSP diagnostics
    fn error_message(&self) -> String {
        format!("{}: {}", self.error_prefix(), self.error_content())
    }

    /// Get the compilation unit for this error, if applicable
    fn compilation_unit(&self) -> Option<PathBuf> {
        None
    }

    /// Get the token start index for this error, if applicable
    fn token_start(&self) -> Option<usize> {
        None
    }

    /// Get the token end index for this error, if applicable
    fn token_end(&self) -> Option<usize> {
        None
    }

    /// Get the byte start for this error, if applicable.
    fn byte_start(&self) -> Option<usize> {
        None
    }

    /// Get the byte end for this error, if applicable.
    fn byte_end(&self) -> Option<usize> {
        None
    }

    /// Get any supplementary notes associated with this error, if applicable.
    fn notes(&self) -> Vec<String> {
        Vec::new()
    }
}

pub struct CXError {
    pub message: String,
}

impl CXErrorTrait for CXError {
    fn pretty_print(&self) {
        println!("CXError: {}", self.message);
    }

    fn error_prefix(&self) -> String {
        "Error".to_string()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }
}

pub type CXResult<T> = Result<T, Box<dyn CXErrorTrait>>;

impl CXError {
    pub fn new<T: Into<String>>(msg: T) -> Self {
        CXError {
            message: msg.into(),
        }
    }

    pub fn unimplemented<T, U: Into<String>>(msg: U) -> CXResult<T> {
        Err(Box::new(CXError::new(format!(
            "Unimplemented: {}",
            msg.into()
        ))))
    }

    pub fn create_result<T, U: Into<String>>(msg: U) -> CXResult<T> {
        Err(Box::new(CXError::new(msg)))
    }

    pub fn create_boxed<U: Into<String>>(msg: U) -> Box<dyn CXErrorTrait> {
        Box::new(CXError::new(msg))
    }
}

impl std::fmt::Debug for CXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CXError: {}", self.message)
    }
}

impl std::fmt::Display for CXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CXError: {}", self.message)
    }
}

#[derive(Clone, Debug)]
pub struct PointingError {
    pub prefix: String,
    pub message: String,
    pub file: PathBuf,
    pub point: usize,
    pub diagnostic_start: usize,
    pub diagnostic_end: usize,
    pub notes: Vec<String>,
}

impl PointingError {
    pub fn new(
        prefix: impl Into<String>,
        message: impl Into<String>,
        file: PathBuf,
        point: usize,
    ) -> Self {
        let diagnostic_end = point.saturating_add(1);
        Self {
            prefix: prefix.into(),
            message: message.into(),
            file,
            point,
            diagnostic_start: point,
            diagnostic_end,
            notes: Vec::new(),
        }
    }

    pub fn with_diagnostic_range(mut self, start: usize, end: usize) -> Self {
        self.diagnostic_start = start;
        self.diagnostic_end = end.max(start.saturating_add(1));
        self
    }
}

impl CXErrorTrait for PointingError {
    fn pretty_print(&self) {
        pretty_point_error(&self.message, self.file.as_path(), self.point);
    }

    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.file.clone())
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.diagnostic_start)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.diagnostic_end)
    }

    fn notes(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Clone, Debug)]
pub struct UnderlineError {
    pub prefix: String,
    pub message: String,
    pub file: PathBuf,
    pub byte_start: usize,
    pub byte_end: usize,
    pub token_start: Option<usize>,
    pub token_end: Option<usize>,
    pub notes: Vec<String>,
}

impl UnderlineError {
    pub fn new(
        prefix: impl Into<String>,
        message: impl Into<String>,
        file: PathBuf,
        byte_start: usize,
        byte_end: usize,
    ) -> Self {
        Self {
            prefix: prefix.into(),
            message: message.into(),
            file,
            byte_start,
            byte_end: byte_end.max(byte_start.saturating_add(1)),
            token_start: None,
            token_end: None,
            notes: Vec::new(),
        }
    }

    pub fn with_token_range(mut self, start: usize, end: usize) -> Self {
        self.token_start = Some(start);
        self.token_end = Some(end);
        self
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }
}

impl CXErrorTrait for UnderlineError {
    fn pretty_print(&self) {
        pretty_underline_error_with_notes(
            &self.error_message(),
            &self.notes,
            self.file.as_path(),
            self.byte_start,
            self.byte_end,
        );
    }

    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.file.clone())
    }

    fn token_start(&self) -> Option<usize> {
        self.token_start
    }

    fn token_end(&self) -> Option<usize> {
        self.token_end
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.byte_start)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.byte_end)
    }

    fn notes(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            eprintln!("Error: {}", msg);

            panic!()
        }
    }
}

#[macro_export]
macro_rules! expr_error_log {
    ($toks:expr, $start:expr, $end:expr, $($arg:tt)*) => {
        {
            use cx_log::log_error;

            eprintln!("{}", $toks[$start .. $end].iter().map(|tok| format!("{}", tok)).collect::<Vec<_>>().join(" "));
            log_error!($($arg)*);
        }
    }
}

#[macro_export]
macro_rules! lmir_error_log {
    ($builder:ident, $($arg:tt)*) => {
        {
            use cx_log::log_error;

            eprintln!("Error in method {}", $builder.current_function_name().unwrap_or("<unknown>"));
            log_error!($($arg)*);
        }
    }
}

pub fn leading_whitespace_count(s: &str) -> usize {
    s.char_indices()
        .take_while(|(_, c)| c.is_whitespace())
        .map(|(index, c)| index + c.len_utf8())
        .last()
        .unwrap_or(0)
}

fn line_as_spacing(line: &str) -> String {
    line.chars()
        .map(|c| if c.is_whitespace() { c } else { ' ' })
        .collect()
}

pub fn get_error_loc(file_contents: &str, index: usize) -> (usize, usize) {
    let mut acc = index.min(file_contents.len());

    for (line_num, line) in file_contents.lines().enumerate() {
        if line.len() + 1 > acc {
            return (line_num + 1, acc);
        }

        acc -= line.len() + 1; // +1 for the newline character
    }

    let last_line = file_contents.lines().count().max(1);
    let last_col = file_contents
        .lines()
        .last()
        .map(|line| line.len())
        .unwrap_or(0);
    (last_line, last_col)
}

fn error_exit() {}

pub fn pretty_underline_error(
    message: &str,
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) {
    pretty_underline_error_with_notes(message, &[], file_path, start_index, end_index);
}

pub fn pretty_underline_error_with_notes(
    message: &str,
    notes: &[String],
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) {
    let Some(file_contents) = std::fs::read_to_string(file_path).ok() else {
        println!("{} (File could not be read)", message);
        return;
    };

    pretty_underline_source_error_with_notes(
        message,
        notes,
        file_path,
        &file_contents,
        start_index,
        end_index,
    );
}

pub fn pretty_underline_source_error(
    message: &str,
    file_path: &Path,
    source: &str,
    start_index: usize,
    end_index: usize,
) {
    pretty_underline_source_error_with_notes(
        message,
        &[],
        file_path,
        source,
        start_index,
        end_index,
    );
}

pub fn pretty_underline_source_error_with_notes(
    message: &str,
    notes: &[String],
    file_path: &Path,
    source: &str,
    start_index: usize,
    end_index: usize,
) {
    let start_index = clamp_to_char_boundary(source, start_index.min(source.len()));
    let end_index = clamp_to_char_boundary(
        source,
        end_index
            .max(start_index.saturating_add(1))
            .min(source.len()),
    );
    let (error_line, error_padding) = get_error_loc(source, start_index);
    let first_line_start = start_index.saturating_sub(error_padding);

    let link = format!(
        "{}:{}:{}",
        file_path
            .canonicalize()
            .map(|s| s.to_string_lossy().as_ref().to_owned())
            .unwrap_or("path parse failure".into()),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t--> {link}");
    for note in notes {
        println!("note: {note}");
    }

    let mut line_start = first_line_start;
    loop {
        let line_end = source[line_start..]
            .find('\n')
            .map(|offset| line_start + offset)
            .unwrap_or(source.len());
        let line = &source[line_start..line_end];
        let underline_start = if line_start == first_line_start {
            start_index.saturating_sub(line_start)
        } else {
            leading_whitespace_count(line)
        }
        .min(line.len());
        let mut underline_end = end_index.min(line_end).saturating_sub(line_start);
        if underline_end <= underline_start {
            underline_end =
                next_char_boundary(line, underline_start).unwrap_or(underline_start + 1);
        }
        let underline_width = underline_end.saturating_sub(underline_start).max(1);
        let lpad = line_as_spacing(&line[..underline_start.min(line.len())]);

        println!("{line}");
        println!("{lpad}{}", "~".repeat(underline_width));

        if end_index <= line_end || line_end == source.len() {
            break;
        }

        line_start = line_end.saturating_add(1);
    }

    error_exit();
}

pub fn pretty_point_error(message: &str, file_path: &Path, index: usize) {
    let file_contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", file_path.to_string_lossy()));

    pretty_point_source_error(message, file_path, &file_contents, index);
}

pub fn pretty_point_source_error(message: &str, file_path: &Path, source: &str, index: usize) {
    let start_index = clamp_to_char_boundary(source, index.min(source.len()));
    let (error_line, error_padding) = get_error_loc(source, start_index);
    let error_line_start = start_index - error_padding;

    let link = format!(
        "{}:{}:{}",
        file_path
            .canonicalize()
            .map(|s| s.to_string_lossy().as_ref().to_owned())
            .unwrap_or("path parse failure".into()),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t --> {link}");

    if let Some(line) = source[error_line_start..].lines().next() {
        let lpad = line_as_spacing(&line[..error_padding]);
        println!("{line}");
        println!("{lpad}^");
    }
}

fn clamp_to_char_boundary(source: &str, mut index: usize) -> usize {
    while index > 0 && !source.is_char_boundary(index) {
        index -= 1;
    }
    index
}

fn next_char_boundary(source: &str, index: usize) -> Option<usize> {
    if index >= source.len() {
        return None;
    }

    source[index..]
        .chars()
        .next()
        .map(|ch| index + ch.len_utf8())
}
