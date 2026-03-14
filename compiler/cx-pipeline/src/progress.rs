use std::time::Instant;

pub struct ProgressReporter {
    total: usize,
    completed: usize,
    start_time: Instant,
    verbose: bool,
    modules_compiled: usize,
}

impl ProgressReporter {
    pub fn new(verbose: bool) -> Self {
        Self {
            total: 0,
            completed: 0,
            start_time: Instant::now(),
            verbose,
            modules_compiled: 0,
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
            eprint!("\r{:<80}\r", "");
        }
    }

    pub fn start_step(&self, step_name: &str, unit_name: &str) {
        if self.verbose {
            eprintln!("[{}/{}] {} {}", self.completed + 1, self.total, step_name, unit_name);
        } else {
            let line = format!("[{}/{}] {} {}...", self.completed + 1, self.total, step_name, unit_name);
            // Pad with spaces to overwrite previous longer lines
            eprint!("\r{:<80}", line);
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

    pub fn link_status(&self, message: &str) {
        if self.verbose {
            eprintln!("{}", message);
        } else {
            eprint!("\r{:<80}", message);
        }
    }

    pub fn finish(&self) {
        let elapsed = self.start_time.elapsed();
        if !self.verbose {
            // Clear the progress line
            eprint!("\r{:<80}\r", "");
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
            eprint!("\r{:<80}\r", "");
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
