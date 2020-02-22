// declarations with call initializer
const constCall = Symbol();
let letCall = Symbol();
var varCall = Symbol();

// ambient declaration with type
declare const constType: unique symbol;

// declaration with type and call initializer
const constTypeAndCall: unique symbol = Symbol();

// type literals
type L = {
    readonly readonlyType: unique symbol;
    nested: {
        readonly readonlyNestedType: unique symbol;
    }
};
declare const l: L;

const constInitToLReadonlyType = l.readonlyType;
const constInitToLReadonlyNestedType = l.nested.readonlyNestedType;
const constInitToLReadonlyTypeWithTypeQuery: typeof l.readonlyType = l.readonlyType;
const constInitToLReadonlyNestedTypeWithTypeQuery: typeof l.nested.readonlyNestedType = l.nested.readonlyNestedType;
const constInitToLReadonlyTypeWithIndexedAccess: L["readonlyType"] = l.readonlyType;
const constInitToLReadonlyNestedTypeWithIndexedAccess: L["nested"]["readonlyNestedType"] = l.nested.readonlyNestedType;

// type argument inference
const promiseForConstCall = Promise.resolve(constCall);
const arrayOfConstCall = [constCall];

// unique symbol widening in expressions
declare const s: unique symbol;
declare namespace N {
    const s: unique symbol;
}
declare const o: { [s]: "a", [N.s]: "b" };

declare function f<T>(x: T): T;

declare function g(x: typeof s): void;
declare function g(x: typeof N.s): void;
