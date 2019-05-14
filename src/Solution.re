module U = UnifierSet;

type t = Seq.t(U.unifier_set);

let to_string = (solution: t): string => {
    switch (solution |> Seq.to_list) {
    | [] => "[]"
    | solution => solution
        |> List.map(unifs => U.string_of_unifier_set(unifs))
        |> String.concat(",\n")
        |> str => {j|[$str]|j}
    }
};

let print = (msg: string, solution: t): t => {
    Js.log(msg ++ ": " ++ to_string(solution));
    solution
};