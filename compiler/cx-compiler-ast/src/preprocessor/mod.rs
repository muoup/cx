mod preprocessor;

pub(crate) struct Preprocessor {
    pub(crate) defined_tokens: Vec<(String, String)>
}

pub fn preprocess(str: &str) -> String {
    let mut preprocessor = Preprocessor {
        defined_tokens: vec![]
    };

    str
        .lines()
        .map(|line| preprocessor::preprocess_line(&mut preprocessor, line))
        .collect()
}