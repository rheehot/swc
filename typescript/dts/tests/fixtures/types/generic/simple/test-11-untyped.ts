const arrayMap = <T, U>(f: (x: T) => U) => (a: T[]) => a.map(f);

const f20 = arrayMap(x => x.length);
