#![feature(box_patterns)]
#![deny(unused_variables)]
#![recursion_limit = "4096"]

#[macro_use]
extern crate pmutil;
extern crate proc_macro;

use pmutil::{Quote, ToTokensExt};
use swc_macros_common::{call_site, print};
use syn::{ExprTryBlock, ImplItemMethod, ReturnType};

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
    let should_return = match i.sig.output {
        ReturnType::Default => false,
        _ => true,
    };

    let try_block = ExprTryBlock {
        attrs: Default::default(),
        try_token: call_site(),
        block: i.block,
    };

    let block = if should_return {
        Quote::new_call_site()
            .quote_with(smart_quote!(
                Vars {
                    try_block: &try_block
                },
                {
                    {
                        let res: Result<_, Error> = try_block;

                        match res {
                            Ok(v) => Ok(v),
                            Err(err) => {
                                self.info.errors.push(err);
                                Err(())
                            }
                        }
                    }
                }
            ))
            .parse()
    } else {
        Quote::new_call_site()
            .quote_with(smart_quote!(
                Vars {
                    try_block: &try_block
                },
                {
                    {
                        let res: Result<_, Error> = try_block;

                        match res {
                            Err(err) => {
                                self.info.errors.push(err);
                            }
                            _ => {}
                        }
                    }
                }
            ))
            .parse()
    };

    ImplItemMethod { block, ..i }
}
