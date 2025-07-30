use std::process::Command;
use cx_data_pipeline::GlobalCompilationContext;

pub(crate) fn link(context: &GlobalCompilationContext) -> Option<()> {
    let mut cmd = Command::new("gcc");
    cmd
        .arg("-no-pie")
        .arg("-o")
        .arg(context.config.output.to_str().expect("Output path invalid"));
    
    let linking_files = context.linking_files.lock().unwrap();
    
    for file in linking_files.iter() {
        cmd.arg(file.to_str().expect("File path invalid"));
    }
    
    let output = cmd.output().ok()?;
    
    if output.status.success() {
        println!("[Linker] Successfully linked files into {}", context.config.output.display());
        Some(())
    } else {
        eprintln!("[Linker] Failed to link files: {}", String::from_utf8_lossy(&output.stderr));
        eprintln!("[Linker] Command: {:?}", cmd);
        None
    }
}