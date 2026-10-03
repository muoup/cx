use std::io::IsTerminal;
use std::time::Instant;

pub struct ProgressReporter {
    start_time: Instant,
    verbose: bool,
    // Redraws a single status line; only when stderr is a terminal and output is not verbose
    interactive: bool,
    modules_compiled: usize,
    last_line_len: usize,
}

impl ProgressReporter {
    pub fn new(verbose: bool) -> Self {
        Self {
            start_time: Instant::now(),
            verbose,
            interactive: !verbose && std::io::stderr().is_terminal(),
            modules_compiled: 0,
            last_line_len: 0,
        }
    }

    /// Clear the progress line so subsequent output (errors, etc.) starts clean.
    pub fn clear_line(&mut self) {
        if self.interactive && self.last_line_len > 0 {
            eprint!("\r{:width$}\r", "", width = self.last_line_len);
            self.last_line_len = 0;
        }
    }

    fn status(&mut self, line: &str) {
        if self.verbose {
            eprintln!("{line}");
        } else if self.interactive {
            let pad_width = self.last_line_len.max(line.len());
            eprint!("\r{:<width$}", line, width = pad_width);
            self.last_line_len = line.len();
        }
    }

    pub fn start_step(&mut self, step_name: &str, unit_name: &str) {
        self.status(&format!("{step_name} {unit_name}"));
    }

    pub fn increment_modules(&mut self) {
        self.modules_compiled += 1;
    }

    pub fn skip_step(&self, unit_name: &str) {
        if self.verbose {
            eprintln!("Skipping {unit_name} (cached)");
        }
    }

    pub fn link_status(&mut self, message: &str) {
        self.status(message);
    }

    pub fn finish(&mut self) {
        self.clear_line();
        eprintln!(
            "Compiled {} module{} in {:.2}s",
            self.modules_compiled,
            if self.modules_compiled == 1 { "" } else { "s" },
            self.start_time.elapsed().as_secs_f64()
        );
    }

    pub fn finish_target(&mut self, target_name: &str) {
        self.clear_line();
        eprintln!(
            "Built target '{}': {} module{} in {:.2}s",
            target_name,
            self.modules_compiled,
            if self.modules_compiled == 1 { "" } else { "s" },
            self.start_time.elapsed().as_secs_f64()
        );
    }
}
