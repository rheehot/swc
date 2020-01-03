use super::{
    scope::{Scope, VarInfo},
    Analyzer,
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
