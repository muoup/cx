pub(crate) fn preprocess_line(string: &str) -> String {
    if string.contains("//") {
        return string.split("//").next().unwrap().to_string();
    }

    if !string.starts_with("#") {
        return string.to_string();
    }

    let mut split = string.split_whitespace();

    match split.next().unwrap() {
        "#include" => {
            let file_name = split.next().unwrap();

            let prefix = if file_name.starts_with("\"") && file_name.ends_with("\"") {
                ""
            } else if file_name.starts_with("<") && file_name.ends_with(">") {
                "lib/libc/"
            } else {
                panic!("Invalid include statement: {}", file_name);
            };

            let path = format!("{}{}", prefix, &file_name[1.. file_name.len() - 1]);
            std::fs::read_to_string(path.as_str())
                .expect(format!("Failed to read file: {}", path).as_str())
        },
        dir => todo!("Preprocessor directive not implemented: {dir}")
    }
}