let (&>) = Belt.Option.flatMap;
let (<$>) = (f, x) => Belt.Option.flatMap(x, f);
let flip = (f, x, y) => f(y, x);

let string_of_string_list = list => {
    list
    |> String.concat(", ")
    |> str => {j|[$str]|j}
}