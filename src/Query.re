module U = UnifierSet;

type fact = U.term;
type knowledge_base = list(fact);
type query = Query(list(U.term));

let rec solve: (query, U.unifier_set, knowledge_base) => list(U.unifier_set)
= (query, unifs, kb) => {
    switch (query) {
    | Query([]) => [unifs]
    | Query([subgoal, ...subgoals]) =>
        kb |> Utils.flat_map(fact => {
            switch (U.unify(fact, subgoal, unifs)) {
            | None => [] // No solution found down this path, so these unifiers shouldn't be shown.
            | Some(new_unifs) => solve(Query(subgoals), new_unifs, kb)
            }
        })
    }
};

let rec vars_in: query => list(string) = fun
| Query([]) => []
| Query([Var(v), ...rest]) => [v, ...vars_in(Query(rest))]
| Query([Pred(_, inner), ...rest]) => vars_in(Query(inner)) @ vars_in(Query(rest))
;

type var_mapping = list((string, U.term))

let string_of_var_mapping: var_mapping => string = vm => {
    vm
    |> List.map(((str, term)) => str ++ " => " ++ U.string_of_term(term))
    |> String.concat(", ")
    |> str => {j|[$str]|j}
};

let string_of_var_mapping_list: list(var_mapping) => string = vml => {
    vml
    |> List.map(string_of_var_mapping)
    |> String.concat(", ")
    |> str => {j|[$str]|j}
};

let mappings: (query, U.unifier_set) => var_mapping = (query, unifs) => {
    let var_names = vars_in(query);
    let key_value = name => {
        let varia = U.Var(name);
        let value = unifs |> U.concretize(varia);
        if (varia == value) {
            None // Would result in cases like "X = X".
        } else {
            Some((name, value))
        }
    };
    var_names
    |> List.sort_uniq(compare)
    |> Belt.List.keepMap(_, key_value)
};

let print_unifier_set_list: (string, list(U.unifier_set)) => list(U.unifier_set)
= (msg, ulist) => {
    if (List.length(ulist) == 0) {
        Js.log(msg ++ ": []");
    } else {
        ulist
        |> List.map(unifs => U.string_of_unifier_set(unifs))
        |> String.concat(",\n")
        |> str => {j|$msg: [$str]|j}
        |> Js.log
    };
    ulist
};

let solve_query = (query: query, kb: knowledge_base) => {
    let Query(query_comps) = query;
    let unifs = kb |> U.from_list |> U.register_all(query_comps);

    let _ = print_unifier_set_list("BEFORE", [unifs]);
    
    solve(query, unifs, kb)
    |> print_unifier_set_list("AFTER")
    |> List.map(mappings(query))
};
