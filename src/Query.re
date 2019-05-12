module U = UnifierSet;

type fact = U.term;
type knowledge_base = list(fact);

type query =
    | Term(U.term)
    | And(list(query))
    | Not(U.term)
    ;

let rec solve = (
    query: query,
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
    switch (query) {
    | Term(term) => solve_term(term, unifs, kb)
    | And(subgoals) => solve_and(subgoals, unifs, kb)
    | Not(subgoal) => [] // TODO: implement
    }
}
and solve_term = (
    term: U.term,
    unifs: U.unifier_set,
    kb: knowledge_base,
) => {
    // For each fact in the knowledge base, try unifying the fact with the
    // term. Only keep the unification results that are non-`None`.
    Belt.List.keepMap(kb, U.unify(_, term, unifs))
}
and solve_and = (
    query: list(query),
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
    switch (query) {
    | [] => [unifs]
    | [subgoal, ...subgoals] =>
        // This first `solve` gives a list of possibilities. We must then try
        // solving `subgoals` using each of the possible `unifier_set`s.
        solve(subgoal, unifs, kb) |> Utils.flat_map(unifs => solve_and(subgoals, unifs, kb))
    };
};

let rec vars_in_query: query => list(string) = fun
| Term(t) => vars_in_term(t)
| And(ts) => vars_in_query_list(ts)
| Not(_) => []
and vars_in_query_list: list(query) => list(string) = fun
| [] => []
| [q, ...qs] => vars_in_query(q) @ vars_in_query_list(qs)
and vars_in_term: U.term => list(string) = fun
| U.Var(v) => [v]
| U.Pred(_, args) => vars_in_term_list(args)
and vars_in_term_list: list(U.term) => list(string) = fun
| [] => []
| [t, ...ts] => vars_in_term(t) @ vars_in_term_list(ts)
;

let mappings: (query, U.unifier_set) => VarMapping.t = (query, unifs) => {
    let var_names = vars_in_query(query);
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

let rec terms_in: query => list(U.term) = fun
| Term(t) => [t]
| And([]) => []
| And([q, ...qs]) => terms_in(q) @ terms_in(And(qs))
| Not(_) => [] // TODO: Implement
;

let solve_query = (query: query, kb: knowledge_base): list(VarMapping.t) => {
    let query_comps = terms_in(query);
    let unifs = kb |> U.from_list |> U.register_all(query_comps);
    solve(query, unifs, kb) |> List.map(mappings(query))
};