const arrayMap = <T, U>(f: (x: T) => U) => (a: T[]) => a.map(f);

const f21 = arrayMap(x => [x]);
