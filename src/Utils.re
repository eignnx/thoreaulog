let (&>) = Belt.Option.flatMap;
let (<$>) = (f, x) => Belt.Option.flatMap(x, f);
let flip = (f, x, y) => f(y, x);

let string_of_string_list = list => {
    list
    |> String.concat(", ")
    |> str => {j|[$str]|j}
}

// Tail recursive!
let flat_map = (f: 'a => list('b), lst: list('a)): list('b) => {
    let rec iter = (lst, acc) => {
        switch (lst) {
        | [] => acc
        | [x, ...xs] => iter(xs, acc @ f(x))
        }
    };
    iter(lst, [])
};