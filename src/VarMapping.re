module U = UnifierSet;

type t = list((string, U.term))

let to_string: t => string = vm => {
    vm
    |> List.map(((str, term)) => str ++ " => " ++ U.string_of_term(term))
    |> String.concat(", ")
    |> str => {j|[$str]|j}
};

let string_of_list: list(t) => string = list => {
    list
    |> List.map(to_string)
    |> String.concat(", ") 
    |> str => {j|[$str]|j}
};