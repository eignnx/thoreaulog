module U = UnifierSet;

type fact = U.term;
type knowledge_base = list(fact);
type query = Query(list(U.term));

let rec solve = (
    query: query,
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
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

let mappings: (query, U.unifier_set) => VarMapping.t = (query, unifs) => {
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

let solve_query = (query: query, kb: knowledge_base): list(VarMapping.t) => {
    let Query(query_comps) = query;
    let unifs = kb |> U.from_list |> U.register_all(query_comps);
    solve(query, unifs, kb) |> List.map(mappings(query))
};
