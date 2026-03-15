use std::time::Instant;

pub struct ProgressReporter {
    total: usize,
    completed: usize,
    start_time: Instant,
    verbose: bool,
    modules_compiled: usize,
    last_line_len: usize,
}

impl ProgressReporter {
    pub fn new(verbose: bool) -> Self {
        Self {
            total: 0,
            completed: 0,
            start_time: Instant::now(),
            verbose,
            modules_compiled: 0,
            last_line_len: 0,
        }
    }

    pub fn set_total(&mut self, total: usize) {
        self.total = total;
    }

    pub fn add_total(&mut self, count: usize) {
        self.total += count;
    }

    /// Clear the progress line so subsequent output (errors, etc.) starts clean.
    pub fn clear_line(&self) {
        if !self.verbose {
            eprint!("\r{:width$}\r", "", width = self.last_line_len);
        }
    }

    pub fn start_step(&mut self, step_name: &str, unit_name: &str) {
        if self.verbose {
            eprintln!("[{}/{}] {} {}", self.completed + 1, self.total, step_name, unit_name);
        } else {
            let line = format!("[{}/{}] {} {}...", self.completed + 1, self.total, step_name, unit_name);
            let pad_width = self.last_line_len.max(line.len());
            eprint!("\r{:<width$}", line, width = pad_width);
            self.last_line_len = line.len();
        }
    }

    pub fn complete_step(&mut self) {
        self.completed += 1;
    }

    pub fn increment_modules(&mut self) {
        self.modules_compiled += 1;
    }

    pub fn skip_step(&self, unit_name: &str) {
        if self.verbose {
            eprintln!("[{}/{}] Skipping {} (cached)", self.completed + 1, self.total, unit_name);
        }
    }

    pub fn link_status(&mut self, message: &str) {
        if self.verbose {
            eprintln!("{}", message);
        } else {
            let pad_width = self.last_line_len.max(message.len());
            eprint!("\r{:<width$}", message, width = pad_width);
            self.last_line_len = message.len();
        }
    }

    pub fn finish(&self) {
        let elapsed = self.start_time.elapsed();
        if !self.verbose {
            // Clear the progress line
            eprint!("\r{:width$}\r", "", width = self.last_line_len);
        }
        eprintln!(
            "Compiled {} module{} in {:.2}s",
            self.modules_compiled,
            if self.modules_compiled == 1 { "" } else { "s" },
            elapsed.as_secs_f64()
        );
    }

    pub fn finish_target(&self, target_name: &str) {
        let elapsed = self.start_time.elapsed();
        if !self.verbose {
            eprint!("\r{:width$}\r", "", width = self.last_line_len);
        }
        eprintln!(
            "Built target '{}': {} module{} in {:.2}s",
            target_name,
            self.modules_compiled,
            if self.modules_compiled == 1 { "" } else { "s" },
            elapsed.as_secs_f64()
        );
    }
}
