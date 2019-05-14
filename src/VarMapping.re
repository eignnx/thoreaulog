module U = UnifierSet;

type t = array((string, U.term));

let to_string: t => string = vm => {
    vm
    |> Array.map(((str, term)) => str ++ " => " ++ U.string_of_term(term))
    |> Array.to_list
    |> String.concat(", ")
    |> str => {j|[$str]|j}
};

let string_of_list: list(t) => string = list => {
    list
    |> List.map(to_string)
    |> String.concat(", ") 
    |> str => {j|[$str]|j}
};