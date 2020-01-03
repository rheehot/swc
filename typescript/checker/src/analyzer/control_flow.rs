use super::{name::Name, Analyzer};
use crate::{
    analyzer::{scope::ScopeKind, util::Comparator},
    errors::Error,
    ty::Type,
};
use fxhash::FxHashMap;
use std::{
    collections::hash_map::Entry,
    convert::TryFrom,
    hash::Hash,
    iter::once,
    mem::replace,
    ops::{AddAssign, BitOr, Not},
};
use swc_ecma_ast::*;

/// Conditional facts
#[derive(Debug, Clone, Default)]
pub(crate) struct CondFacts {
    pub facts: FxHashMap<Name, TypeFacts>,
    pub vars: FxHashMap<Name, Type<'static>>,
    pub excludes: FxHashMap<Name, Vec<Type<'static>>>,
    pub types: FxHashMap<JsWord, Type<'static>>,
}

impl CondFacts {
    fn extend(&mut self, other: Self) {
        self.facts.extend(other.facts);
        self.vars.extend(other.vars);
        self.types.extend(other.types);
    }

    fn or<K, T>(mut map: FxHashMap<K, T>, map2: FxHashMap<K, T>) -> FxHashMap<K, T>
    where
        K: Eq + Hash,
        T: Merge,
    {
        for (k, v) in map2 {
            match map.entry(k) {
                Entry::Occupied(mut e) => {
                    e.get_mut().or(v);
                }
                Entry::Vacant(e) => {
                    e.insert(v);
                }
            }
        }

        map
    }
}

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

        let l = replace(self, Type::never(l_span));

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

impl Analyzer<'_> {
    pub(super) fn validate_if_stmt(&mut self, s: &IfStmt) -> Result<(), Error> {
        let mut facts = Default::default();
        match self.detect_facts(&stmt.test, &mut facts) {
            Ok(()) => (),
            Err(err) => {
                self.info.errors.push(err);
                return stmt;
            }
        };
        let ends_with_ret = stmt.cons.ends_with_ret();
        let stmt = self.with_child(ScopeKind::Flow, facts.true_facts, |child| {
            child.validate_stmt(&stmt)
        });
        if ends_with_ret {
            self.scope.facts.extend(facts.false_facts);
        }

        stmt
    }

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
