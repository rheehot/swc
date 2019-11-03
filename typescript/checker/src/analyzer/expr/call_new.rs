use swc_common::{Visit, VisitWith};
use swc_ecma_ast::*;

use crate::{analyzer::Analyzer, errors::Error, ty::Type};

impl Visit<NewExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &NewExpr) {
        e.visit_children(self);

        let res: Result<(), Error> = try {
            let callee_ty = self.type_of(&e.callee)?;
            match *callee_ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                }) if e.type_args.is_some() => Err(Error::TS2347 { span: e.span })?,
                _ => {}
            }
        };

        if let Err(err) = res {
            self.info.errors.push(err);
        }

        // Check arguments
        if let Some(ref args) = e.args {
            for arg in args {
                let res: Result<(), Error> = try {
                    self.type_of(&arg.expr)?;
                };

                if let Err(err) = res {
                    self.info.errors.push(err);
                }
            }
        }
    }
}

impl Visit<CallExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &CallExpr) {
        e.visit_children(self);

        // Check arguments
        for arg in &e.args {
            let res: Result<(), Error> = try {
                self.type_of(&arg.expr)?;
            };

            if let Err(err) = res {
                self.info.errors.push(err);
            }
        }

        // Check callee
        let res: Result<(), Error> = try {
            if let ExprOrSuper::Expr(ref callee) = e.callee {
                let callee_ty = self.type_of(&callee);
                let callee_ty = match callee_ty {
                    Ok(v) => v,
                    Err(_) => return,
                };
                match *callee_ty.normalize() {
                    Type::Keyword(TsKeywordType {
                        kind: TsKeywordTypeKind::TsAnyKeyword,
                        ..
                    }) if e.type_args.is_some() => Err(Error::TS2347 { span: e.span })?,
                    _ => {}
                }
            }
        };

        if let Err(err) = res {
            self.info.errors.push(err);
        }
    }
}
