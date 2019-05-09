let (&>) = Belt.Option.flatMap;
let (<$>) = (f, x) => Belt.Option.flatMap(x, f);
let flip = (f, x, y) => f(y, x);