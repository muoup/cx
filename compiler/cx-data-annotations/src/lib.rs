use std::collections::HashMap;

pub struct TypeAnnotation {
    pub name: String,
    pub assertion: String
}

pub type AnnotationMap = HashMap<String, TypeAnnotation>;