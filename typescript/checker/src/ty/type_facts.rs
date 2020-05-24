use super::Type;
use swc_ecma_ast::TsKeywordTypeKind;

impl Type {
    pub(crate) fn apply_type_facts(self, facts: TypeFacts) -> Type {
        let keyword_types = &[
            (
                TypeFacts::TypeofEQString,
                TypeFacts::TypeofNEQString,
                TsKeywordTypeKind::TsStringKeyword,
            ),
            (
                TypeFacts::TypeofEQNumber,
                TypeFacts::TypeofNEQNumber,
                TsKeywordTypeKind::TsNumberKeyword,
            ),
            (
                TypeFacts::TypeofEQBoolean,
                TypeFacts::TypeofNEQBoolean,
                TsKeywordTypeKind::TsBooleanKeyword,
            ),
            (
                TypeFacts::TypeofEQBigInt,
                TypeFacts::TypeofNEQBigInt,
                TsKeywordTypeKind::TsBigIntKeyword,
            ),
            (
                TypeFacts::TypeofEQSymbol,
                TypeFacts::TypeofNEQSymbol,
                TsKeywordTypeKind::TsSymbolKeyword,
            ),
        ];
    }
}
