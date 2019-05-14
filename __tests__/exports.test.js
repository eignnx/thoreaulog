const {
    Term,
    Query,
    KnowledgeBase,
} = require("../src/exports");

describe("`Term` constructors and methods", () => {
    test("work for single atom", () => {
        const term = Term.Atom("adam");
        expect(Term.toString(term)).toBe("adam");
    });

    test("work for compound predicate", () => {
        const term = Term.Pred("likes", [Term.Atom("jane"), Term.Atom("carrie")]);
        expect(Term.toString(term)).toBe("likes(jane, carrie)");
    });

    test("work for variables", () => {
        const term = Term.Pred("likes", [Term.Atom("jane"), Term.Var("Who")]);
        expect(Term.toString(term)).toBe("likes(jane, Who)");
    });
});

describe("`Query` constructors and methods", () => {
    test("work for single term", () => {
        const pred = Term.Pred("likes", [Term.Atom("jane"), Term.Atom("carrie")]);
        const query = Query.Term(pred);
        expect(Query.toString(query)).toBe("likes(jane, carrie)");
    });

    test("work for `Not` query", () => {
        const pred = Term.Pred("likes", [Term.Atom("jane"), Term.Atom("carrie")]);
        const query = Query.Not(Query.Term(pred));
        expect(Query.toString(query)).toBe("not(likes(jane, carrie))");
    });

    test("work for `And` query", () => {
        const pred = Term.Pred("likes", [Term.Atom("jane"), Term.Atom("carrie")]);
        const query = Query.And([
            Query.Term(pred),
            Query.Term(Term.Atom("my_fact")),
            Query.Not(Query.Term(Term.Atom("other_fact"))),
        ]);
        expect(Query.toString(query)).toBe("(likes(jane, carrie), my_fact, not(other_fact))");
    });
});

describe("`KnowledgeBase` objects", () => {
    let likes = (a, b) => Term.Pred("likes", [Term.Atom(a), Term.Atom(b)]);

    const kb = new KnowledgeBase([
        likes("carrie", "darcy"),
        likes("darcy", "carrie"),
    ]);

    likes = (a, b) => Term.Pred("likes", [a, b])

    test("solve correctly", () => {
        const query = Query.And([
            likes(Term.Var("X"), Term.Var("Y")),
            likes(Term.Var("Y"), Term.Var("X")),
        ].map(Query.Term));

        const ans = [
            {
                "X": Term.Atom("carrie"),
                "Y": Term.Atom("darcy"),
            },
            {
                "X": Term.Atom("darcy"),
                "Y": Term.Atom("carrie"),
            },
        ];

        expect(kb.query(query)).toEqual(ans);
    });
});