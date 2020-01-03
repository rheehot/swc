use crate::{errors::Error, legacy::Analyzer, ty::Type};
use swc_common::{Fold, FoldWith, Spanned};
use swc_ecma_ast::*;

impl Fold<UnaryExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, node: UnaryExpr) -> UnaryExpr {
        log_fold!(node);

        let node = node.fold_children(self);

        let mut errors = vec![];

        match node.op {
            op!("typeof") | op!("delete") | op!("void") => match self.type_of(&node.arg) {
                Ok(ref ty) => match ty.normalize() {
                    Type::EnumVariant(..) if node.op == op!("delete") => {
                        errors.push(Error::TS2704 {
                            span: node.arg.span(),
                        })
                    }

                    _ => {}
                },

                Err(err) => errors.push(err),
            },

            op!("~") => match self.type_of(&node.arg) {
                Ok(ref ty) => match ty.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => {}

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => errors.push(Error::TS2531 { span: ty.span() }),

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    }) => errors.push(Error::TS2532 { span: ty.span() }),

                    _ => {
                        //
                    }
                },

                Err(err) => errors.push(err),
            },

            op!(unary, "-") | op!(unary, "+") => match self.type_of(&node.arg) {
                Ok(ref ty) => match ty.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNumberKeyword,
                        ..
                    }) => {}

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsNullKeyword,
                        ..
                    }) => errors.push(Error::TS2531 { span: ty.span() }),

                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsUndefinedKeyword,
                        ..
                    }) => errors.push(Error::TS2532 { span: ty.span() }),

                    _ => {
                        //
                    }
                },

                Err(err) => errors.push(err),
            },

            _ => {}
        }

        self.info.errors.extend(errors);

        node
    }
}
