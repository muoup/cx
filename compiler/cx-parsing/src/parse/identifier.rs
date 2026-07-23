use cx_ast::ast::{
    expression::{CXExprKind, CXExpression},
    template::CXTemplateInput,
    types::{CXType, CXTypeKind, PredeclarationType},
};
use cx_log::{
    error::{context::CXInternalContext, message::CXStdErrMessage, CXErr},
    CXResult,
};
use cx_tokens::{
    operator,
    token::{OperatorType, TokenKind},
    TokenIter,
};
use cx_util::{
    identifier::CXIdent,
    namespace::{EnvironmentNamespace, NamespacePath, QualifiedName},
};

use crate::{log::parse_point_error, next_kind, try_next};

use super::{parser::ParserData, templates::parse_template_args, types::is_type_decl};

#[derive(Debug, Clone)]
pub(crate) struct ParsedIdentifier {
    pub name: QualifiedName,
    pub template_input: Option<CXTemplateInput>,
}

impl ParsedIdentifier {
    pub(crate) fn new(name: QualifiedName, template_input: Option<CXTemplateInput>) -> Self {
        Self {
            name,
            template_input,
        }
    }

    pub(crate) fn into_type(self, predeclaration: PredeclarationType) -> CXType {
        CXTypeKind::Identifier {
            name: self.name,
            predeclaration,
            template_input: self.template_input,
        }
        .to_type()
    }

    pub(crate) fn into_expr(
        self,
        start_index: usize,
        end_index: usize,
        file_origin: EnvironmentNamespace,
    ) -> CXExpression {
        CXExprKind::Identifier {
            name: self.name,
            template_input: self.template_input,
        }
        .into_expr(start_index, end_index, file_origin)
    }

    #[allow(dead_code)]
    pub(crate) fn into_qualified_name(self) -> CXResult<QualifiedName> {
        if self.template_input.is_some() {
            return Err(CXErr::new(
                CXStdErrMessage::error("PARSER ERROR", "Expected non-templated identifier"),
                CXInternalContext::error(
                    "non-templated identifier conversion has no active parser token context",
                ),
            ));
        }

        Ok(self.name)
    }
}

pub(crate) fn try_parse_identifier(data: &mut ParserData) -> CXResult<Option<ParsedIdentifier>> {
    let Some(name) = try_parse_qualified_name(&mut data.tokens)? else {
        return Ok(None);
    };

    let template_input = if matches!(
        data.tokens.peek().map(|token| &token.kind),
        Some(TokenKind::Operator(OperatorType::Less))
    ) {
        data.tokens.next();
        let is_template = is_type_decl(data)?;
        data.tokens.back();

        if is_template {
            Some(parse_template_args(data)?)
        } else {
            None
        }
    } else {
        None
    };

    Ok(Some(ParsedIdentifier::new(name, template_input)))
}

pub(crate) fn try_parse_type_identifier(
    data: &mut ParserData,
) -> CXResult<Option<ParsedIdentifier>> {
    let Some(name) = try_parse_qualified_name(&mut data.tokens)? else {
        return Ok(None);
    };

    let template_input = if matches!(
        data.tokens.peek().map(|token| &token.kind),
        Some(TokenKind::Operator(OperatorType::Less))
    ) {
        Some(parse_template_args(data)?)
    } else {
        None
    };

    Ok(Some(ParsedIdentifier::new(name, template_input)))
}

pub(crate) fn try_parse_qualified_name(tokens: &mut TokenIter) -> CXResult<Option<QualifiedName>> {
    if !matches!(
        tokens.peek().map(|token| &token.kind),
        Some(TokenKind::Identifier(_))
    ) {
        return Ok(None);
    };

    let mut segments = Vec::new();

    loop {
        let TokenKind::Identifier(ident) = next_kind!(tokens)? else {
            return parse_point_error(
                tokens,
                "Expected identifier after '::' in qualified name".to_string(),
            );
        };

        segments.push(CXIdent::new(ident.clone()));

        if !try_next!(tokens, operator!(ScopeRes)) {
            break;
        }
    }

    let ident = segments
        .pop()
        .expect("identifier parser should have at least one segment");
    Ok(Some(QualifiedName::new(
        NamespacePath::new(segments),
        ident,
    )))
}
