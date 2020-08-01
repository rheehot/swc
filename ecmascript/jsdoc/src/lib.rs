#![deny(unused)]

pub use self::input::Input;
use crate::ast::*;
use nom::{
    bytes::complete::{tag, take_while},
    character::is_alphabetic,
    IResult,
};
use swc_ecma_ast::Str;

pub mod ast;
mod input;

pub fn parse(i: Input) -> IResult<Input, JsDoc> {}

pub fn parse_tag_item(i: Input) -> IResult<Input, JsDocTagItem> {
    let (i, _) = tag("@")(i)?;

    let (mut i, tag_name) = take_while(is_alphabetic)(i)?;

    let span = tag_name.span();

    let tag = match tag_name {
        "abstract" | "virtual" => JsDocTag::Abstract(JsDocAbstractTag { span }),

        "access" => {
            let (input, access) = parse_one_of(i, &["private", "protected", "package", "public"])?;
            i = input;
            JsDocTag::Access(JsDocAccessTag {
                span,
                access: access.into(),
            })
        }

        "alias" => {
            let (input, name_path) = parse_name_path(i)?;
            i = input;
            JsDocTag::Alias(JsDocAliasTag { span, name_path })
        }

        "async" => JsDocTag::Async(JsDocAsyncTag { span }),

        "augments" | "extends" => {
            let (input, name_path) = parse_name_path(i)?;
            i = input;
            JsDocTag::Augments(JsDocAugmentsTag {
                span,
                class: name_path,
            })
        }

        "author" => {
            let (input, author) = parse_line(i)?;
            i = input;
            JsDocTag::Author(JsDocAuthorTag { span, author })
        }

        "borrows" => {
            let (input, from) = parse_name_path(i)?;
            let (input, _) = tag("as")(input);
            let (input, to) = parse_name_path(input)?;
            i = input;
            JsDocTag::Borrows(JsDocBorrowsTag { span, from, to })
        }

        "callback" => {
            let (input, name_path) = parse_name_path(i)?;
            i = input;
            JsDocTag::Callback(JsDocCallbackTag { span, name_path })
        }

        "class" | "constructor" => {
            // TODO: name is must if ty is some
            let (input, ty) = parse_opt_str(i)?;
            let (input, name) = parse_opt_str(input)?;
            i = input;

            JsDocTag::Class(JsDocClassTag { span, ty, name })
        }

        "classdesc" => {
            let (input, desc) = parse_line(i)?;
            i = input;

            JsDocTag::ClassDesc(JSDocClassDescTag { span, desc })
        }

        "constant" | "const" => {
            // TODO: name is must if ty is some
            let (input, ty) = parse_opt_str(i)?;
            let (input, name) = parse_opt_str(input)?;
            i = input;
            JsDocTag::Const(JsDocConstTag { span, ty, name })
        }

        "constructs" => {
            let (input, name) = parse_line(i)?;
            i = input;
            JsDocTag::Constructs(JsDocConstructsTag { span, name })
        }

        "copyright" => {
            let (input, text) = parse_line(i)?;
            i = input;
            JsDocTag::Copyright(JsDocCopyrightTag { span, text })
        }

        "default" | "defaultvalue" => {
            let (input, value) = parse_line(i)?;
            i = input;
            JsDocTag::Default(JsDocDefaultTag { span, value })
        }

        "deprecated" => {
            let (input, text) = parse_line(i)?;
            i = input;
            JsDocTag::Deprecated(JsDocDeprecatedTag { span, text })
        }

        "description" | "desc" => {
            let (input, text) = parse_line(i)?;
            i = input;
            JsDocTag::Description(JsDocDescriptionTag { span, text })
        }

        "enum" => {
            let (input, ty) = parse_type(i)?;
            i = input;
            JsDocTag::Enum(JsDocEnumTag { span, ty })
        }

        "event" => {
            // TODO: implement this
            let (input, ty) = parse_line(i)?;
            i = input;
            JsDocTag::Unknown(JsDocUnknownTag { span, extras: ty })
        }

        "example" => {}

        "exports" => {}

        "external" => {}

        "file" => {}

        "fires" => {}

        "function" => {}

        "generator" => {}

        "global" => {}

        "hideconstructor" => {}

        "ignore" => {}

        "implements" => {}

        "inheritdoc" => {}

        "inner" => {}

        "instance" => {}

        "interface" => {}

        "kind" => {}

        "lends" => {}

        "license" => {}

        "listens" => {}

        "member" | "var" => {}

        "memberof" => {}

        "mixes" => {}

        "mixin" => {}

        "module" => {}

        "name" => {}

        "namespace" => {}

        "override" => {}

        "package" => {}

        "param" | "arg" | "argument" => {}

        "private" => {}

        "property" | "prop" => {}

        "protected" => {}

        "public" => {}

        "readonly" => {}

        "requires" => {}
        "returns" | "return" => {}

        "see" => {}

        "since" => {}

        "static" => {}

        "summary" => {}

        "this" => {}

        "tutorial" => {}

        "type" => {}

        "typedef" => {}

        "variation" => {}

        "version" => {}

        "yields" | "yield" => {}

        _ => {
            let (input, extras) = parse_str(i);
            i = input;
            JsDocTag::Unknown(JsDocUnknownTag { span, extras })
        }
    };

    Ok((
        i,
        JsDocTagItem {
            span,
            tag_name: tag_name.into(),
            tag,
        },
    ))
}

fn parse_one_of<'i, 'l>(i: Input<'i>, list: &'l [&str]) -> IResult<Input<'i>, &'l str> {
    for s in list {
        if i.starts_with(s) {
            let i = i[s.len()..];
            return Ok((i, s));
        }
    }

    Err(nom::Err::Error((
        format!("Expected one of {}", list.join(",")).as_bytes(),
        nom::error::ErrorKind::Tag,
    )))
}

fn parse_name_path(i: Input) -> IResult<Input, &str> {}

fn parse_line(i: Input) -> IResult<Input, Str> {
    let res = i.src.char_indices().find(|(_, c)| c == '\n' || c == '\r');

    if let Some((idx, _)) = res {
        let ret = &i.src[..idx];

        Ok((
            i,
            Str {
                span: i.start,
                value: Default::default(),
                has_escape: false,
            },
        ))
    } else {
        Ok((i, ""))
    }
}

fn parse_opt_str(i: Input) -> IResult<Input, &str> {}

fn parse_str(i: Input) -> IResult<Input, &str> {}

fn parse_type(i: Input) -> IResult<Input, &str> {}
