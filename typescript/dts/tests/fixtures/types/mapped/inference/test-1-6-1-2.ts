// @strictNullChecks: true
// @noimplicitany: true
// @declaration: true

declare type Box<T> = {
    value: T;
};
declare type Boxified<T> = {
    [P in keyof T]: Box<T[P]>;
};

declare function box<T>(x: T): Box<T>;

declare function unbox<T>(x: Box<T>): T;

declare function boxify<T>(obj: T): Boxified<T>;

declare function unboxify<T>(obj: Boxified<T>): T;

declare function assignBoxified<T>(obj: Boxified<T>, values: T): void;

declare function makeDictionary<D>(obj: {
    [x: string]: D;
}): {
    [x: string]: D;
};

declare var s: string;

declare let b: {
    [x: string]: Box<number> | Box<string> | Box<boolean>;
};
let v = unboxify(b);
