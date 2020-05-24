function error1(message: string): never {
    throw new Error(message);
}

function fail1() {
    return error1("Something failed");
}

function check<T>(x: T | undefined) {
    return x || error1("Undefined value");
}


function f1(x: string | number) {
    if (typeof x === "boolean") {
        x;  // never
    }
}

function f2(x: string | number) {
    while (true) {
        if (typeof x === "boolean") {
            return x;  // never
        }
    }
}

