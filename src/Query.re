module U = UnifierSet;

type fact = U.term;
type query = list(U.term);

let rec (&>) = (lst: list('a), f: 'a => list('b)): list('b) => {
    switch (lst) {
    | [] => []
    | [x, ...xs] => f(x) @ (xs &> f)
    };
};

let rec solve: (query, U.unifier_set, list(fact)) => list(U.unifier_set)
= (query, unifs, kb) => {
    switch (query) {
    | [] => [unifs]
    | [subgoal, ...subgoals] =>
        kb &> fact => switch(unifs |> U.unify(fact, subgoal)) {
        | None => []
        | Some(new_unifs) =>
            solve(subgoals, new_unifs, kb)
        }
    }
};

let rec vars_in: query => list(string) = fun
| [] => []
| [Var(v), ...rest] => [v, ...vars_in(rest)]
| [Pred(_, inner), ...rest] => vars_in(inner) @ vars_in(rest)
;

type var_mapping = list((string, U.term))

let mappings: (query, U.unifier_set) => var_mapping = (query, unifs) => {
    let var_names = vars_in(query);
    let key_value = name => (name, unifs |> U.find_root(Var(name)));
    var_names |> List.map(key_value)
};

let solve_query = (query, kb) => {
    let unifs = kb |> U.from_list |> U.register_all(query);
    solve(kb, unifs, query) |> List.map(mappings(query))
};
