use crate::preprocessor::Preprocessor;

fn handle_non_directive(preprocessor: &mut Preprocessor, string: &str) -> String {
    let mut result = string.to_string();

    for (token, value) in preprocessor.defined_tokens.iter() {
        result = result.replace(format!("{}", token).as_str(), value);
    }

    result
}

pub(crate) fn preprocess_line(preprocessor: &mut Preprocessor, mut string: &str) -> String {
    if string.contains("/*") {
        if string.contains("*/") {
            let pre = string.split_once("/*").unwrap().0;
            let post = string.split_once("*/").unwrap().1;

            return format!("{}{}", preprocess_line(preprocessor, pre), preprocess_line(preprocessor, post));
        }

        string = string.split_once("/*").unwrap().0;
        preprocessor.in_ml_comment = true;
    }

    if preprocessor.in_ml_comment {
        if string.contains("*/") {
            string = string.rsplit_once("*/")
                .unwrap()
                .1;
            preprocessor.in_ml_comment = false;
        } else {
            return "".to_string();
        }
    }
    
    if string.contains("//") {
        string = string.split("//").next().unwrap();
    }

    if !string.trim_start().starts_with("#") {
        return handle_non_directive(preprocessor, string);
    }

    let mut split = string.split_whitespace();

    match split.next().unwrap() {
        "#include" => {
            let file_name = split.next().unwrap();

            let prefix = if file_name.starts_with("\"") && file_name.ends_with("\"") {
                "".to_string()
            } else if file_name.starts_with("<") && file_name.ends_with(">") {
                let mut path = std::env::current_exe()
                    .expect("Failed to get current executable path")
                    .parent()
                    .expect("Failed to get parent directory of executable")
                    .to_str()
                    .expect("Failed to convert path to string")
                    .to_string();
                path.push_str("/lib/libc/");
                
                path
            } else {
                panic!("Invalid include statement: {}", file_name);
            };

            let path = format!("{}{}", prefix, &file_name[1.. file_name.len() - 1]);
            let string = std::fs::read_to_string(path.as_str())
                .expect(format!("Failed to read file: {path}").as_str());

            string.lines()
                .map(|line| preprocess_line(preprocessor, line))
                .collect::<Vec<_>>()
                .join("\n")
        },
        "#define" => {
            let token = split.next().unwrap();
            let value = split.collect::<Vec<_>>().join(" ");

            preprocessor.defined_tokens.push((token.to_string(), value.to_string()));
            "".to_string()
        },
        dir => todo!("Preprocessor directive not implemented: {dir}")
    }
}