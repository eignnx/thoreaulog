let (&>) = Belt.Option.flatMap;
let flip = (f, x, y) => f(y, x);