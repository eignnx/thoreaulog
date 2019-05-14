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
): Seq.t(array((string, U.term))) => {
    Q.validate(query);
    let query_vars = Q.vars_in_query(query);
    let into_pairs = Q.mappings(query_vars);
    Q.solve(query, unifs, kb) |> Seq.map(into_pairs)
};
// </QUERY> //////////////////////////////////////////////////////////

// <UNIFIERSET> ///////////////////////////////////////////////////////////
let register_facts = U.from_list;
let register_fact = U.register;
let regiter_query = Q.register_query;
// </UNIFIERSET> //////////////////////////////////////////////////////////

let cons = (x, xs) => [x, ...xs];

let sequence_next: Seq.t('a) => option(('a, Seq.t('a))) = seq => {
    switch (seq()) {
    | Seq.Nil => None
    | Seq.Cons(x, next) => Some((x, next))
    }
};