use super::{
    scope::{Scope, ScopeKind, VarInfo},
    type_facts::TypeFacts,
    Analyzer, Name,
};
use crate::{
    errors::Error,
    ty::{Intersection, Tuple, Type, TypeRef, TypeRefExt, Union},
    util::{EqIgnoreNameAndSpan, IntoCow},
};
use fxhash::FxHashMap;
use std::{
    borrow::Cow,
    collections::hash_map::Entry,
    convert::TryFrom,
    hash::Hash,
    iter::once,
    mem,
    ops::{AddAssign, BitOr, Not},
};
use swc_atoms::JsWord;
use swc_common::{Fold, FoldWith, Span, Spanned};
use swc_ecma_ast::*;

#[derive(Debug, Default)]
struct Facts {
    true_facts: CondFacts,
    false_facts: CondFacts,
}

impl Not for Facts {
    type Output = Self;
    #[inline]
    fn not(self) -> Self {
        Facts {
            true_facts: self.false_facts,
            false_facts: self.true_facts,
        }
    }
}

impl AddAssign for Facts {
    fn add_assign(&mut self, rhs: Self) {
        self.true_facts += rhs.true_facts;
        self.false_facts += rhs.false_facts;
    }
}

impl AddAssign<Option<Self>> for Facts {
    fn add_assign(&mut self, rhs: Option<Self>) {
        match rhs {
            Some(rhs) => {
                *self += rhs;
            }
            None => {}
        }
    }
}

impl BitOr for Facts {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        Facts {
            true_facts: self.true_facts | rhs.true_facts,
            false_facts: self.false_facts | rhs.false_facts,
        }
    }
}

trait Merge {
    fn or(&mut self, other: Self);
}

impl<T> Merge for Vec<T> {
    fn or(&mut self, other: Self) {
        self.extend(other)
    }
}

impl Merge for TypeFacts {
    fn or(&mut self, other: Self) {
        *self |= other
    }
}

impl Merge for VarInfo {
    fn or(&mut self, other: Self) {
        self.copied |= other.copied;
        self.initialized |= other.initialized;
        Merge::or(&mut self.ty, other.ty);
    }
}

impl Merge for Type<'_> {
    fn or(&mut self, r: Self) {
        let l_span = self.span();

        let l = mem::replace(self, Type::never(l_span));

        *self = Type::union(once(l).chain(once(r)));
    }
}

impl<T> Merge for Option<T>
where
    T: Merge,
{
    fn or(&mut self, other: Self) {
        match *self {
            Some(ref mut v) => match other {
                Some(other) => v.or(other),
                None => {}
            },
            _ => *self = other,
        }
    }
}

impl AddAssign for CondFacts {
    fn add_assign(&mut self, rhs: Self) {
        self.types.extend(rhs.types);
        self.vars.extend(rhs.vars);
        self.excludes.extend(rhs.excludes);
    }
}

impl AddAssign<Option<Self>> for CondFacts {
    fn add_assign(&mut self, rhs: Option<Self>) {
        match rhs {
            Some(rhs) => {
                *self += rhs;
            }
            None => {}
        }
    }
}

impl BitOr for CondFacts {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        CondFacts {
            facts: CondFacts::or(self.facts, rhs.facts),
            vars: CondFacts::or(self.vars, rhs.vars),
            types: CondFacts::or(self.types, rhs.types),
            excludes: CondFacts::or(self.excludes, rhs.excludes),
        }
    }
}

impl Analyzer<'_, '_> {
    #[inline]
    fn child(&self, kind: ScopeKind, facts: CondFacts) -> Analyzer {
        let mut child = Analyzer::new(
            self.libs,
            self.rule,
            Scope::new(&self.scope, kind, facts),
            self.path.clone(),
            self.loader,
        );
        child.top_level = false;
        child.allow_ref_declaring = self.allow_ref_declaring;
        child.span_allowed_implicit_any = self.span_allowed_implicit_any;
        if !self.return_type_span.is_dummy() {
            child.return_type_span = self.return_type_span;
            child
                .inferred_return_types
                .get_mut()
                .insert(self.return_type_span, vec![]);
        }

        child
    }

    pub(super) fn with_child<Ret, F>(&mut self, kind: ScopeKind, facts: CondFacts, op: F) -> Ret
    where
        F: for<'a, 'b> FnOnce(&mut Analyzer<'a, 'b>) -> Ret,
    {
        let mode = self.computed_prop_mode;
        let ret;
        let (errors, ret_types) = {
            let mut child = self.child(kind, facts);
            child.computed_prop_mode = mode;

            ret = op(&mut child);

            assert_eq!(
                child.info.exports,
                Default::default(),
                "Child node cannot export"
            );
            (
                child.info.errors,
                mem::replace(child.inferred_return_types.get_mut(), Default::default()),
            )
        };

        self.info.errors.extend(errors);

        self.inferred_return_types.get_mut().extend(ret_types);

        ret
    }
}

impl Analyzer<'_, '_> {
    pub(super) fn try_assign(&mut self, span: Span, lhs: &PatOrExpr, ty: &Type) {
        let res: Result<(), Error> = try {
            match *lhs {
                PatOrExpr::Expr(ref expr) | PatOrExpr::Pat(box Pat::Expr(ref expr)) => {
                    let lhs_ty = self.type_of_expr(expr, TypeOfMode::LValue, None)?;

                    self.assign(&lhs_ty, &ty, span)?;

                    match **expr {
                        // TODO: Validate
                        Expr::Member(MemberExpr { .. }) => return,
                        _ => unimplemented!(
                            "assign: {:?} = {:?}\nFile: {}",
                            expr,
                            ty,
                            self.path.display()
                        ),
                    }
                }

                PatOrExpr::Pat(ref pat) => {
                    self.try_assign_pat(span, pat, ty)?;
                }
            }
        };

        match res {
            Ok(()) => {}
            Err(err) => self.info.errors.push(err),
        }
    }

    fn try_assign_pat(&mut self, span: Span, lhs: &Pat, ty: &Type) -> Result<(), Error> {
        // Update variable's type
        match *lhs {
            Pat::Ident(ref i) => {
                if let Some(ref var_info) = self.scope.get_var(&i.sym) {
                    if let Some(ref var_ty) = var_info.ty {
                        // let foo: string;
                        // let foo = 'value';
                        self.assign(&var_ty, ty, i.span)?;
                        return Ok(());
                    }
                }

                {
                    if let Some(var_info) = self.scope.vars.get_mut(&i.sym) {
                        let var_ty = ty;

                        if var_info.ty.is_none()
                            || (!var_info.ty.as_ref().unwrap().is_any()
                                && !var_info.ty.as_ref().unwrap().is_unknown())
                        {
                            //                            var_info.ty =
                            // Some(var_ty);
                        }
                        return Ok(());
                    } else {
                        let var_info = if let Some(var_info) = self.scope.search_parent(&i.sym) {
                            let ty = if var_info.ty.is_some()
                                && var_info.ty.as_ref().unwrap().is_any()
                            {
                                Some(Type::any(var_info.ty.as_ref().unwrap().span()))
                            } else if var_info.ty.is_some()
                                && var_info.ty.as_ref().unwrap().is_unknown()
                            {
                                // Type narrowing
                                Some(ty.to_static())
                            } else {
                                return Ok(());
                            };

                            VarInfo {
                                ty,
                                copied: true,
                                ..var_info.clone()
                            }
                        } else {
                            if let Some(Type::Module(..)) = self.scope.find_type(&i.sym) {
                                return Err(Error::NotVariable {
                                    span: i.span,
                                    left: lhs.span(),
                                });
                            }

                            if self.allow_ref_declaring && self.declaring.contains(&i.sym) {
                                return Ok(());
                            } else {
                                // undefined symbol
                                return Err(Error::UndefinedSymbol { span: i.span });
                            }
                        };

                        // Variable is defined on parent scope.
                        //
                        // We copy varinfo with enhanced type.
                        println!(
                            "({}) vars.insert({}, {:?})",
                            self.scope.depth(),
                            i.sym,
                            var_info
                        );
                        self.scope.vars.insert(i.sym.clone(), var_info);

                        return Ok(());
                    }
                }
            }

            Pat::Array(ref arr) => {
                //
                for (i, elem) in arr.elems.iter().enumerate() {
                    if let Some(elem) = elem.as_ref() {
                        match *ty {
                            Type::Tuple(Tuple { ref types, .. }) => {
                                if types.len() > i {
                                    self.try_assign_pat(span, elem, &types[i])?;
                                }
                            }

                            _ => unimplemented!(
                                "assignment with array pattern\nPat: {:?}\nType: {:?}",
                                lhs,
                                ty
                            ),
                        }
                    }
                }
                return Ok(());
            }

            _ => {}
        }

        unimplemented!(
            "assignment with complex pattern\nPat: {:?}\nType: {:?}",
            lhs,
            ty
        )
    }

    fn add_true_false(&self, facts: &mut Facts, sym: &JsWord, ty: &Type<'static>) {
        facts.insert_var(sym, ty.clone(), false);
    }

    /// Returns (type facts when test is matched, type facts when test is not
    /// matched)
    fn detect_facts(&self, test: &Expr, facts: &mut Facts) -> Result<(), Error> {
        match *test {
            // Useless
            Expr::Fn(..)
            | Expr::Arrow(..)
            | Expr::Lit(Lit::Bool(..))
            | Expr::Lit(Lit::Str(..))
            | Expr::Lit(Lit::Null(..))
            | Expr::Lit(Lit::Num(..))
            | Expr::MetaProp(..)
            | Expr::JSXFragment(..)
            | Expr::JSXNamespacedName(..)
            | Expr::JSXEmpty(..) => return Ok(()),

            Expr::Call(..) => {
                let ty = self.type_of(&test)?;
                match *ty.normalize() {
                    Type::Simple(ref sty) => match **sty {
                        TsType::TsTypePredicate(ref pred) => {
                            //
                            let name = Name::from(&pred.param_name);
                            let ty = Type::from(pred.type_ann.clone());
                            facts.insert_var(name.clone(), ty.clone(), false);
                        }
                        _ => {}
                    },
                    _ => {}
                }

                return Ok(());
            }

            // Object literal *may* have side effect.
            Expr::Object(..) => {}

            // Array literal *may* have side effect.
            Expr::Array(..) => {}

            Expr::Await(AwaitExpr { arg: ref expr, .. })
            | Expr::TsNonNull(TsNonNullExpr { ref expr, .. }) => {
                self.detect_facts(expr, facts)?;
            }

            Expr::Seq(SeqExpr { ref exprs, .. }) => {
                for expr in exprs {
                    self.detect_facts(expr, facts)?;
                }
            }

            Expr::Paren(ParenExpr { ref expr, .. }) => self.detect_facts(expr, facts)?,

            Expr::Ident(ref i) => {
                let ty = self.type_of(test)?.to_static();
                self.add_true_false(facts, &i.sym, &ty);
            }

            Expr::Bin(BinExpr {
                op: op!("&&"),
                ref left,
                ref right,
                ..
            }) => {
                // order is important
                self.detect_facts(left, facts)?;
                self.detect_facts(right, facts)?;
            }

            Expr::Bin(BinExpr {
                op: op!("||"),
                ref left,
                ref right,
                ..
            }) => {
                let (mut l_facts, mut r_facts) = Default::default();
                self.detect_facts(&left, &mut l_facts)?;
                self.detect_facts(&right, &mut r_facts)?;

                *facts += l_facts | r_facts;

                return Ok(());
            }

            Expr::Bin(BinExpr {
                op,
                ref left,
                ref right,
                ..
            }) => {
                let l_ty = self.type_of(left)?;
                let r_ty = self.type_of(right)?;

                match op {
                    op!("===") | op!("!==") | op!("==") | op!("!=") => {
                        let is_eq = op == op!("===") || op == op!("==");

                        let c = Comparator {
                            left: &**left,
                            right: &**right,
                        };

                        // Check typeof a === 'string'
                        {
                            match c.take(|l, r| match l {
                                Expr::Unary(UnaryExpr {
                                    op: op!("typeof"),
                                    ref arg,
                                    ..
                                }) => match r {
                                    Expr::Lit(Lit::Str(Str { ref value, .. })) => Some((
                                        Name::try_from(&**arg),
                                        if is_eq {
                                            (
                                                TypeFacts::typeof_eq(&*value),
                                                TypeFacts::typeof_neq(&*value),
                                            )
                                        } else {
                                            (
                                                TypeFacts::typeof_neq(&*value),
                                                TypeFacts::typeof_eq(&*value),
                                            )
                                        },
                                    )),
                                    _ => None,
                                },
                                _ => None,
                            }) {
                                Some((Ok(name), (Some(t), Some(f)))) => {
                                    // Add type facts

                                    facts.true_facts.facts.insert(name.clone(), t);
                                    facts.false_facts.facts.insert(name.clone(), f);
                                    return Ok(());
                                }
                                _ => {}
                            }
                        }

                        // Try narrowing type
                        let c = Comparator {
                            left: (&**left, &l_ty),
                            right: (&**right, &r_ty),
                        };

                        match c.take(|(l, l_ty), (_, r_ty)| match **l_ty {
                            Type::Keyword(TsKeywordType {
                                kind: TsKeywordTypeKind::TsUnknownKeyword,
                                ..
                            }) => {
                                //
                                Some((Name::try_from(l), r_ty.to_static()))
                            }
                            _ => None,
                        }) {
                            Some((Ok(name), ty)) => {
                                if is_eq {
                                    facts.insert_var(name.clone(), ty.clone(), false);
                                } else {
                                    facts.insert_var(name.clone(), ty.clone(), true);
                                }
                                return Ok(());
                            }
                            _ => {}
                        }

                        return Ok(());
                    }

                    op!("instanceof") => {
                        match **left {
                            Expr::Ident(ref i) => {
                                //
                                let r_ty = self.expand_type(right.span(), r_ty)?;

                                facts
                                    .true_facts
                                    .vars
                                    .insert(Name::from(&i.sym), r_ty.to_static());

                                return Ok(());
                            }

                            _ => {}
                        }
                    }

                    _ => {}
                }

                unimplemented!("detect_facts({:?})", test)
            }

            Expr::Unary(UnaryExpr {
                op: op!("!"),
                ref arg,
                ..
            }) => {
                let mut f = Default::default();
                self.detect_facts(&arg, &mut f)?;
                *facts += !f;
            }

            _ => unimplemented!("detect_facts({:?})", test),
        }

        Ok(())
    }
}

impl Analyzer<'_, '_> {
    ///
    /// - `span`: Span of the return statement.
    pub(super) fn visit_return_arg(&mut self, span: Span, arg: Option<&Expr>) {
        println!(
            "RET! visit_return_arg: {:?}; key={:?}",
            span, self.return_type_span
        );

        let ty = match arg {
            Some(ref expr) => {
                // let span = expr.span();
                match self
                    .type_of(&expr)
                    .and_then(|ty| self.expand_type(span, ty))
                {
                    Ok(ty) => ty.to_static().respan(span),
                    Err(err) => {
                        self.info.errors.push(err);
                        return;
                    }
                }
            }
            None => Type::undefined(span),
        };

        let key = self.return_type_span;
        // key is dummy for top-level returns.
        if !key.is_dummy() {
            let v = vec![];
            let dup = self
                .inferred_return_types
                .borrow()
                .get(&key)
                .unwrap_or_else(|| &v)
                .iter()
                .any(|t| t.eq_ignore_name_and_span(&ty));
            if !dup {
                self.inferred_return_types
                    .get_mut()
                    .entry(key)
                    .or_default()
                    .push(ty);
            }
        }
    }
}

impl Fold<SwitchStmt> for Analyzer<'_, '_> {
    fn fold(&mut self, stmt: SwitchStmt) -> SwitchStmt {
        let stmt = stmt.fold_children(self);

        analyze!(self, {
            let discriminant_ty = self.type_of(&stmt.discriminant)?;
            for case in &stmt.cases {
                if let Some(ref test) = case.test {
                    let case_ty = self.type_of(&test)?;
                    let case_ty = self.expand_type(case.span(), case_ty)?;
                    self.assign(&case_ty, &discriminant_ty, test.span())?
                }
            }
        });

        let mut false_facts = CondFacts::default();
        let mut true_facts = CondFacts::default();
        // Declared at here as it's important to know if last one ends with return.
        let mut ends_with_ret = false;
        let len = stmt.cases.len();
        let stmt_span = stmt.span();

        let mut cases = Vec::with_capacity(len);
        let mut errored = false;
        // Check cases *in order*
        for (i, mut case) in stmt.cases.into_iter().enumerate() {
            if errored {
                cases.push(case);
                continue;
            }

            let span = case
                .test
                .as_ref()
                .map(|v| v.span())
                .unwrap_or_else(|| stmt_span);

            let SwitchCase { cons, .. } = case;
            let last = i == len - 1;
            let mut facts = Default::default();

            ends_with_ret = cons.ends_with_ret();

            match case.test {
                Some(ref test) => {
                    match self.detect_facts(
                        &Expr::Bin(BinExpr {
                            op: op!("==="),
                            span,
                            left: stmt.discriminant.clone(),
                            right: test.clone(),
                        }),
                        &mut facts,
                    ) {
                        Ok(()) => {}
                        Err(err) => {
                            self.info.errors.push(err);
                            errored = true;
                            cases.push(SwitchCase { cons, ..case });
                            continue;
                        }
                    }
                }
                None => {}
            }

            true_facts = true_facts | facts.true_facts;
            case.cons = self.with_child(ScopeKind::Flow, true_facts.clone(), |child| {
                cons.fold_with(child)
            });
            false_facts += facts.false_facts;

            if ends_with_ret || last {
                true_facts = CondFacts::default();
                true_facts += false_facts.clone();
            }

            cases.push(case);
        }

        if ends_with_ret {
            self.scope.facts.extend(false_facts);
        }

        SwitchStmt { cases, ..stmt }
    }
}

impl Fold<CondExpr> for Analyzer<'_, '_> {
    fn fold(&mut self, e: CondExpr) -> CondExpr {
        let CondExpr { alt, cons, .. } = e;

        let mut facts = Default::default();
        match self.detect_facts(&e.test, &mut facts) {
            Ok(()) => (),
            Err(err) => {
                self.info.errors.push(err);
                return CondExpr { cons, alt, ..e };
            }
        };
        let test = e.test.fold_with(self);
        let cons = self.with_child(ScopeKind::Flow, facts.true_facts, |child| {
            cons.fold_with(child)
        });
        let alt = self.with_child(ScopeKind::Flow, facts.false_facts, |child| {
            alt.fold_with(child)
        });

        CondExpr {
            test,
            cons,
            alt,
            ..e
        }
    }
}

impl Facts {
    fn insert_var<N: Into<Name>>(&mut self, name: N, ty: Type<'static>, negate: bool) {
        let name = name.into();

        if negate {
            self.false_facts.vars.insert(name.clone(), ty.clone());
            self.true_facts.excludes.entry(name).or_default().push(ty);
        } else {
            self.true_facts.vars.insert(name.clone(), ty.clone());
            self.false_facts.excludes.entry(name).or_default().push(ty);
        }
    }
}
