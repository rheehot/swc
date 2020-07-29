use crate::ast::{
    JsDoc, JsDocAbstractTag, JsDocAccessTag, JsDocAliasTag, JsDocAsyncTag, JsDocAugmentsTag,
    JsDocAuthorTag, JsDocBorrowsTag, JsDocTag, JsDocTagItem, JsDocUnknownTag,
};
use nom::{
    bytes::complete::{tag, take_while},
    character::is_alphabetic,
    IResult,
};
use swc_atoms::JsWord;
use swc_common::{BytePos, Span};

pub mod ast;

pub fn parse(start: BytePos, end: BytePos, i: &str) -> IResult<&str, JsDoc> {}

pub fn parse_tag_item(start: BytePos, end: BytePos, i: &str) -> IResult<&str, JsDocTagItem> {
    let (i, _) = tag("@")(i)?;

    let (mut i, tag_name) = take_while(is_alphabetic)(i)?;

    let span = Span::new(start, start + BytePos(tag_name.0 as _), Default::default());

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

        "callback" => {}

        "class" | "constructor" => {}

        "classdesc" => {}

        "constant" | "const" => {}

        "constructs" => {}

        "copyright" => {}

        "default" | "defaultvalue" => {}

        "deprecated" => {}

        "description" | "desc" => {}

        "enum" => {}

        "event" => {}

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

fn parse_one_of(i: &str, list: &[&str]) -> IResult<&str, &str> {
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
