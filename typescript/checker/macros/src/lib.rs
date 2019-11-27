#![feature(box_patterns)]
#![deny(unused_variables)]
#![recursion_limit = "4096"]

#[macro_use]
extern crate pmutil;
extern crate proc_macro;
#[macro_use]
extern crate quote;

use pmutil::{Quote, ToTokensExt};
use proc_macro2::Span;
use std::{collections::HashMap, fs::read_dir, path::Path, sync::Arc};
use swc_macros_common::{call_site, print};
use syn::{punctuated::Punctuated, ImplItemMethod, LitStr, Token};

/// This macro converts
///
/// ```ignore
/// 
/// impl Foo {
///     #[validator]
///     fn validate_foo(&mut self, arg: Arg1) -> Result<Ret, ()> {
///         // body
///         Err(err)?;
///     }
/// }
/// ```
///
/// to
///
///
/// ```ignore
/// 
/// impl Foo {
///     fn validate_foo(&mut self, arg: Arg1) -> Result<Ret, ()> {
///         let res: Result<Ret, Error> = try {
///             // body
///             Err(err)?
///         };
///
///         match res {
///             Ok(v) => Ok(v),
///             Err(err) => {
///                 self.info.errors.push(err);
///                 Err(())
///             }
///         }
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn validator(
    _: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item = syn::parse(item).expect("failed to parse input as an item");
    let item = expand_validator(item);
    print("validator", item.dump())
}

fn expand_validator(i: ImplItemMethod) -> ImplItemMethod {
    let block = Quote::new_call_site()
        .quote_with(smart_quote!(Vars { block: &i.block }, {
            let res: Result<_, Error> = try { block };

            match res {
                Ok(v) => Ok(v),
                Err(err) => {
                    self.info.errors.push(err);
                    Err(())
                }
            }
        }))
        .parse();

    ImplItemMethod { block, ..i }
}
