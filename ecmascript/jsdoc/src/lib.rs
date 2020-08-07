pub use self::input::Input;
use crate::ast::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::is_alphabetic,
    IResult, InputIter, Slice,
};
use swc_common::{Span, Spanned};
use swc_ecma_ast::Str;

pub mod ast;
mod input;

pub fn parse(i: Input) -> IResult<Input, JsDoc> {}

pub fn parse_tag_item(i: Input) -> IResult<Input, JsDocTagItem> {
    let (i, _) = tag("@")(i)?;

    let (mut i, tag_name) = parse_word(i)?;

    let span = tag_name.span();

    let tag = match &*tag_name.value {
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
            let (input, _) = tag("as")(input)?;
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

        "example" => {
            let (input, text) = take_while(|c| c != '@')(i)?;
            i = input;
            JsDocTag::Example(JsDocExampleTag {
                span,
                text: text.into(),
            })
        }

        "exports" => {
            let (input, text) = parse_line(i)?;
            i = input;
            JsDocTag::Exports(JsDocExportsTag {
                span,
                module_name: text.into(),
            })
        }

        "external" | "host" => {
            let (input, name) = parse_line(i)?;
            i = input;
            JsDocTag::External(JsDocExternalTag {
                span,
                name: name.into(),
            })
        }

        "file" | "fileoverview" | "overview" => {
            let (input, text) = parse_line(i)?;
            i = input;
            JsDocTag::File(JsDocFilelTag { span, text })
        }

        "fires" | "emits" => {
            // TODO: implement this
            let (input, ty) = parse_line(i)?;
            i = input;
            JsDocTag::Unknown(JsDocUnknownTag { span, extras: ty })
        }

        "function" | "func" | "method" => {
            let (input, name) = parse_opt_str(i)?;
            i = input;
            JsDocTag::Function(JsDocFunctionTag { span, name })
        }

        "generator" => JsDocTag::Generator(JsDocGeneratorTag { span }),

        "hideconstructor" => JsDocTag::HideConstructor(JsDocHideConstructorTag { span }),

        "ignore" => JsDocTag::Ignore(JsDocIgnoreTag { span }),

        "implements" => {
            let (input, class) = parse_type(i)?;
            i = input;
            JsDocTag::Implements(JsDocImplementsTag { span, class })
        }

        "inheritdoc" => JsDocTag::InheritDoc(JsDocInheritDocTag { span }),

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
            let (input, extras) = parse_str(i)?;
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

fn parse_opt_str(i: Input) -> IResult<Input, Option<Str>> {
    let (i, res) = parse_line(i)?;

    if res.value.is_empty() {
        Ok((i, None))
    } else {
        Ok((i, Some(res)))
    }
}

fn parse_str(i: Input) -> IResult<Input, Str> {
    parse_line(i)
}

fn parse_type(i: Input) -> IResult<Input, Str> {
    parse_line(i)
}

fn parse_one_of<'i>(i: Input<'i>, list: &[&str]) -> IResult<Input<'i>, Str> {
    for &item in list {
        if i.starts_with(item) {
            let res = tag(item)(i);
            match res {
                Ok(v) => return Ok((v.0, v.1.into())),
                Err(..) => continue,
            }
        }
    }

    Err()
}

// ----- ----- Done ----- -----

fn parse_name_path(mut i: Input) -> IResult<Input, JsDocNamePath> {
    let lo = i.span().lo;
    let mut components = vec![];

    loop {
        let (input, component) = parse_word(i)?;
        i = input;

        let (input, _) = match tag(".")(i) {
            Ok(v) => v,
            Err(err) => {
                if components.is_empty() {
                    return Err(err);
                }

                return Ok((
                    i,
                    JsDocNamePath {
                        span: Span::new(lo, i.span().hi, Default::default()),
                        components,
                    },
                ));
            }
        };
    }
}

fn parse_word(i: Input) -> IResult<Input, Str> {
    let res = i
        .iter_indices()
        .find(|(_, c)| !(('a' <= *c && *c <= 'z') || ('A' <= *c && *c <= 'Z')));

    if let Some((idx, _)) = res {
        let rest = i.slice(idx + 1..);
        let ret = i.slice(..idx);

        Ok((rest, ret.into()))
    } else {
        // Everything was alphabet
        Ok((Input::empty(), i.into()))
    }
}

fn parse_line(i: Input) -> IResult<Input, Str> {
    let res = i.iter_indices().find(|(_, c)| *c == '\n' || *c == '\r');

    if let Some((idx, _)) = res {
        let rest = i.slice(idx + 1..);
        let ret = i.slice(..idx);

        Ok((rest, ret.into()))
    } else {
        Ok((i, Input::empty().into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use swc_common::BytePos;

    fn input(s: &str) -> Input {
        Input::new(BytePos(0), BytePos(s.as_bytes().len() as _), s)
    }

    #[test]
    fn test_parse_line() {
        let (rest, ret) = parse_line(input("foo bar\nbaz")).unwrap();

        assert_eq!(&*ret.value, "foo bar");
        assert_eq!(&*rest, "baz");
    }

    #[test]
    fn test_parse_word() {
        let (rest, ret) = parse_word(input("foo bar\nbaz")).unwrap();

        assert_eq!(&*ret.value, "foo");
        assert_eq!(&*rest, "bar\nbaz");
    }
}
