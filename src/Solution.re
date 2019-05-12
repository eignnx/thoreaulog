module U = UnifierSet;

type t = list(U.unifier_set);

let to_string = (solution: t): string => {
    switch (solution) {
    | [] => "[]"
    | _ => solution
        |> List.map(unifs => U.string_of_unifier_set(unifs))
        |> String.concat(",\n")
        |> str => {j|[$str]|j}
    }
};

let print = (msg: string, solution: t): t => {
    Js.log(msg ++ ": " ++ to_string(solution));
    solution
};