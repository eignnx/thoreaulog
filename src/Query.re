module U = UnifierSet;

type fact = U.term;
type knowledge_base = list(fact);

type query =
    | Term(U.term)
    | And(list(query))
    | Not(query)
    ;

let rec solve = (
    query: query,
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
    switch (query) {
    | Term(term) => solve_term(term, unifs, kb)
    | And(subgoals) => solve_and(subgoals, unifs, kb)
    | Not(subgoal) => solve_not(subgoal, unifs, kb)
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
}
and solve_not = (
    query: query,
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
    switch (solve(query, unifs, kb)) {
    | [] => [unifs] // Is this correct?
    | _ => [] // OPTIMIZE: We only need one counter-example, so if ONE is found we can stop.
    }
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
| Not(q) => terms_in(q)
;

exception Invalid_query(string);

let validate = query => {
    let rec iter = (q: query, seen_vars: list(string)) => {
        switch (q) {
        | Term(_) | And([]) => ()
        | And([q, ...qs]) =>
            // First, recurse into `q` with `seen_vars`.
            iter(q, seen_vars);
            // Then check `qs`, but use the vars defined in `q` as well.
            iter(And(qs), vars_in_query(q) @ seen_vars)
        | Not(q) =>
            if (Utils.(seen_vars <: vars_in_query(q))) {
                let msg = "Variables must be bound BEFORE appearing in a `not` predicate!";
                raise(Invalid_query(msg))
            } else {
                iter(q, seen_vars)
            }
        }
    };
    iter(query, []);
};

let solve_query = (query: query, kb: knowledge_base): list(VarMapping.t) => {
    validate(query);
    let query_comps = terms_in(query);
    let unifs = kb |> U.from_list |> U.register_all(query_comps);
    solve(query, unifs, kb) |> List.map(mappings(query))
};