mod preprocessor;

pub fn preprocess(str: &str) -> String {
    str
        .lines()
        .map(|line| preprocessor::preprocess_line(line))
        .collect()
}