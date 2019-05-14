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
    const kb = new KnowledgeBase();
    kb.addFact("likes", ["darcy", "carrie"]);
    kb.addFact("likes", ["carrie", "darcy"]);

    const likes = (a, b) => Query.Term(Term.Pred("likes", [a, b]));
    const [X, Y] = [Term.Var("X"), Term.Var("Y")];

    const q = Query.And([
        likes(X, Y), likes(Y, X)
    ]);

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

    test("solve correctly when extracting answers one-by-one", () => {
        kb.query(q);

        const actual = [
            kb.answer(),
            kb.answer(),
        ];

        expect(actual).toEqual(ans);
    });

    test("solve correctly when extracting all answers at once", () => {
        kb.query(q);
        expect(kb.allAnswers()).toEqual(ans);
    });
});