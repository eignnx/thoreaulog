module U = UnifierSet;

type fact = U.term;
type knowledge_base = list(fact);

type query =
    | Term(U.term)
    | And(list(query))
    | Not(query)
    ;

let rec string_of_query: query => string = fun
| Term(t) => U.string_of_term(t)
| And(qs) =>
    qs
    |> List.map(string_of_query)
    |> String.concat(", ")
    |> str => "(" ++ str ++ ")"
| Not(q) => "not(" ++ string_of_query(q) ++ ")"
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
    // For each fact in the knowledge base, try unifying the `term` with the
    // fact. Only keep the unification results that are non-`None`.
    kb |> Seq.of_list |> Seq.filter_map(fact => unifs |> U.unify(fact, term))
}
and solve_and = (
    query: list(query),
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
    switch (query) {
    | [] => Seq.return(unifs)
    | [subgoal, ...subgoals] =>
        // This first `solve` gives a list of possibilities. We must then try
        // solving `subgoals` using each of the possible `unifier_set`s.
        solve(subgoal, unifs, kb) |> Seq.flat_map(unifs => solve_and(subgoals, unifs, kb))
    };
}
and solve_not = (
    query: query,
    unifs: U.unifier_set,
    kb: knowledge_base,
): Solution.t => {
    let solns = solve(query, unifs, kb);
    switch (solns()) {
    | Seq.Nil => Seq.return(unifs)
    | _ => Seq.empty
    }
};

module StrSet = Set.Make({
    type t = string;
    let compare = compare;
});

let rec vars_in_query: query => StrSet.t = fun
| Term(t) => vars_in_term(t)
| And(ts) => vars_in_query_list(ts)
| Not(_) => StrSet.empty
and vars_in_query_list: list(query) => StrSet.t = fun
| [] => StrSet.empty
| [q, ...qs] => StrSet.union(vars_in_query(q), vars_in_query_list(qs))
and vars_in_term: U.term => StrSet.t = fun
| U.Var(v) => StrSet.singleton(v)
| U.Pred(_, args) => vars_in_term_list(args)
and vars_in_term_list: list(U.term) => StrSet.t = fun
| [] => StrSet.empty
| [t, ...ts] => StrSet.union(vars_in_term(t), vars_in_term_list(ts))
;

let mappings: (StrSet.t, U.unifier_set) => VarMapping.t = (var_names, unifs) => {
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
    |> StrSet.elements
    |> Belt.List.keepMap(_, key_value)
    |> Array.of_list
};

let rec terms_in: query => list(U.term) = fun
| Term(t) => [t]
| And([]) => []
| And([q, ...qs]) => terms_in(q) @ terms_in(And(qs))
| Not(q) => terms_in(q)
;

exception Invalid_query(string);

let validate = query => {
    let rec iter = (q: query, seen_vars: StrSet.t) => {
        switch (q) {
        | Term(_) | And([]) => ()
        | And([q, ...qs]) =>
            // First, recurse into `q` with `seen_vars`.
            iter(q, seen_vars);
            // Then check `qs`, but use the vars defined in `q` as well.
            iter(And(qs), StrSet.union(vars_in_query(q), seen_vars))
        | Not(q) =>
            if (!StrSet.subset(vars_in_query(q), seen_vars)) {
                let msg = "Variables must be bound BEFORE appearing in a `not` predicate!";
                raise(Invalid_query(msg))
            } else {
                iter(q, seen_vars)
            }
        }
    };
    iter(query, StrSet.empty)
};

let register_query = (query: query, unifs: U.unifier_set): U.unifier_set => {
    unifs |> U.register_all(terms_in(query))
};

let solve_query = (query: query, kb: knowledge_base): Seq.t(VarMapping.t) => {
    validate(query);
    let unifs = kb |> U.from_list |> register_query(query);
    let query_vars = vars_in_query(query);
    solve(query, unifs, kb) |> Seq.map(mappings(query_vars))
};