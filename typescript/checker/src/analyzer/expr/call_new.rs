use swc_common::{Fold, FoldWith};
use swc_ecma_ast::*;

use crate::{analyzer::Analyzer, errors::Error, ty::Type};

impl Fold<NewExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, e: NewExpr) -> NewExpr {
        log_fold!(e);

        let e = e.fold_children(self);

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

        e
    }
}

impl Fold<CallExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, e: CallExpr) -> CallExpr {
        log_fold!(e);

        let e = e.fold_children(self);

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
                    Err(_) => return e,
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

        e
    }
}
