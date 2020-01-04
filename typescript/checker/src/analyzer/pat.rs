use super::Analyzer;
use crate::{errors::Error, ty::Type, util::EqIgnoreNameAndSpan};
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

impl Analyzer<'_> {
    fn visit_rest_pat(&mut self, p: &RestPat) {
        let p = p.fold_children(self);
impl Visit<RestPat> for Analyzer<'_> {
impl Visit<RestPat> for Analyzer<'_, '_> {
    fn visit(&mut self, p: &RestPat) {
        let p = p.visit_children(self);

        let mut errors = vec![];
        let p = p.visit_children(self);

        if let Pat::Assign(AssignPat { ref right, .. }) = *p.arg {
            try {
                let value_ty = self.validate_expr(right)?;

                match value_ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(Error::TS2370 { span: p.dot3_token })?,
                }
            }
            .store(&mut errors);
        } else if let Some(ref type_ann) = p.type_ann {
            try {
                let ty =
                    self.expand_type(p.span(), Type::from(type_ann.clone().type_ann).owned())?;

                match *ty.normalize() {
                    Type::Array(..)
                    | Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) => {}
                    _ => Err(Error::TS2370 { span: p.dot3_token })?,
                }
            }
            .store(&mut errors);
        }

        self.info.errors.extend(errors);

        p
    }
}

impl Analyzer<'_> {
    fn visit_assign_pat(&mut self, p: &AssignPat) {
        let p = p.fold_children(self);
impl Visit<AssignPat> for Analyzer<'_> {
impl Visit<AssignPat> for Analyzer<'_, '_> {
    fn visit(&mut self, p: &AssignPat) {
        let p = p.visit_children(self);

        //
        match *p.left {
            Pat::Object(ref left) => {
                //
                match *p.right {
                    Expr::Object(ref right) => {
                        'l: for e in &right.props {
                            match e {
                                PropOrSpread::Prop(ref prop) => {
                                    //
                                    for lp in &left.props {
                                        match lp {
                                            ObjectPatProp::KeyValue(KeyValuePatProp {
                                                key: ref pk,
                                                ..
                                            }) => {
                                                //
                                                match **prop {
                                                    Prop::KeyValue(KeyValueProp {
                                                        ref key,
                                                        ..
                                                    }) => {
                                                        if pk.eq_ignore_name_and_span(key) {
                                                            continue 'l;
                                                        }
                                                    }
                                                    _ => {}
                                                }
                                            }
                                            _ => {}
                                        }
                                    }

                                    self.info.errors.push(Error::TS2353 { span: prop.span() })
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {
                        // TODO: Report an error
                    }
                }
            }
            _ => {}
        }

        p
    }
}