module Q = Query;
module U = UnifierSet;

let knowledge_base_of_array: array(Q.fact) => Q.knowledge_base = Array.to_list;

// <TERM> ///////////////////////////////////////////////////////////
let atom = U.atom;

let var_ = (name: string): U.term => U.Var(name);

let pred = (name: string, args: array(U.term)): U.term => {
    U.Pred(name, Array.to_list(args))
};

let string_of_term = U.string_of_term;
// </TERM> //////////////////////////////////////////////////////////

// <QUERY> ///////////////////////////////////////////////////////////
let term = (t: U.term): Q.query => {
    Q.Term(t)
};

let and_ = (args: array(Q.query)): Q.query => {
    Q.And(Array.to_list(args))
};

let not_ = (q: Q.query): Q.query => {
    Q.Not(q)
};

let string_of_query = Q.string_of_query;

let solve = (
    query: Q.query,
    unifs: U.unifier_set,
    kb: Q.knowledge_base,
): array(array((string, U.term))) => {
    Q.solve(query, unifs, kb)
    |> List.map(soln => {
        soln
        |> Q.mappings(query)
        |> Array.of_list
    })
    |> Array.of_list
};
// </QUERY> //////////////////////////////////////////////////////////

// <UNIFIERSET> ///////////////////////////////////////////////////////////
let register_facts = U.from_list;
let regiter_query = Q.register_query;
// </UNIFIERSET> //////////////////////////////////////////////////////////