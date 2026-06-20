pub trait CXErrorContext {
    fn as_string(&self) -> String;
}

pub struct CXPointingError {
    file: PathBuf,
    str_index: usize
}

impl CXPointingError {
    pub fn new(file: PathBuf, str_index: usize) -> Self {
        Self { file, str_index }
    }

    pub fn file(&self) -> &PathBuf {
        &self.file
    }

    pub fn str_index(&self) -> usize {
        self.str_index
    }    
}

pub struct CXUnderlineError {
    file: PathBuf,
    str_start: usize,
    str_end: usize,    
}

impl CXUnderlineError {
    pub fn new(file: PathBuf, str_start: usize, str_end: usize) -> Self {
        Self { file, str_start, str_end }
    }

    pub fn file(&self) -> &PathBuf {
        &self.file
    }

    pub fn str_start(&self) -> usize {
        self.str_start
    }

    pub fn str_end(&self) -> usize {
        self.str_end
    }    
}

pub struct CXUnspannedError {
    prefix: String,
    message: String,
    notes: Vec<String>,
}

pub fn UnspannedError(prefix: &str, message: &str) -> CXUnspannedError {
    CXUnspannedError {
        prefix: prefix.to_string(),
        message: message.to_string(),
        notes: Vec::new(),
    }
}