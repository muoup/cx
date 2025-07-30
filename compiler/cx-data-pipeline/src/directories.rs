use std::path::PathBuf;

pub fn file_path(path: &str) -> String {
    if path.starts_with("std") {
        let current_exe = std::env::current_exe()
            .expect("Failed to get current executable path");

        if cfg!(feature = "test") {
            format!("{}/../../../lib/{}", current_exe.parent().unwrap().display(), &path)
        } else {
            format!("{}/../../lib/{}", current_exe.parent().unwrap().display(), &path)
        }
    } else {
        format!("{path}")
    }
}

pub fn internal_directory(path: &PathBuf) -> PathBuf {
    PathBuf::from(".internal").join(path)
}