open Jest;
open Expect;
open! Expect.Operators;

open Query;
open UnifierSet;

let likes = (a, b) => Pred("likes", [a, b]);
let kb: knowledge_base = [
    likes(atom("john"), atom("jane")),
    likes(atom("jane"), atom("carrey")),
    likes(atom("ted"), atom("matt")),
    likes(atom("jane"), atom("ted")),
];

describe("Expect", () => {
    test("no substitutions for an empty query", () => {
        let query: query = And([]);
        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual([[]])
    });

    test("one answer for simple query", () => {
        let kb = [
            likes(atom("ted"), atom("matt")),
        ];

        let query = And([
            likes(atom("ted"), Var("X")) -> Term
        ]);

        let expected = [
            [("X", atom("matt"))]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(expected)
    });

    test("two-part, disconnected query to work", () => {
        let query = And([
            likes(atom("ted"), Var("X")) -> Term,
            likes(Var("Y"), atom("carrey")) -> Term,
        ]);

        let ans = [
            [
                ("X", atom("matt")),
                ("Y", atom("jane")),
            ]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });

    test("two-part, connected query to work", () => {
        let query = And([
            likes(Var("X"), Var("Y")) -> Term,
            likes(Var("Y"), atom("ted")) -> Term,
        ]);

        let ans = [
            [
                ("X", atom("john")),
                ("Y", atom("jane")),
            ]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });
});

describe("Expect to find multiple solutions", () => {
    test("in simple case", () => {
        let query = And([
            likes(atom("jane"), Var("X")) -> Term,
        ]);
        
        let ans = [
            [
                ("X", atom("carrey"))
            ],
            [
                ("X", atom("ted"))
            ]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });
});

describe("Expect a long string of subqueries", () => {

    let long_query = [
        likes(atom("john"), Var("A")) -> Term,  // A = jane
        likes(Var("A"), Var("B")) -> Term,      // B = ted
        likes(Var("B"), atom("matt")) -> Term,
    ];

    test("to work", () => {
        let query = And(long_query);

        let ans = [
            [
                ("A", atom("jane")),
                ("B", atom("ted")),
            ]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });

    test("with extraneous fact to work", () => {
        let query = And([
            likes(Var("A"), atom("carrey")) -> Term, // Extraneous fact!
            ...long_query
        ]);

        let ans = [
            [
                ("A", atom("jane")),
                ("B", atom("ted")),
            ]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });
});

describe("Expect queries on family tree knowledgebase", () => {
    let kb: knowledge_base = {
        let parent = (p, c) => Pred("parent", [atom(p), atom(c)]);
        [
            parent("shmi", "anakin"),
            parent("anakin", "luke"),
            parent("anakin", "leah"),
            parent("padme", "luke"),
            parent("padme", "leah"),
            parent("leah", "kylo"),
            parent("han", "kylo"),
        ]
    };

    let parent = (p, c) => Pred("parent", [p, c]);
    let sort = {
        let cmp_fst = ((x, _), (y, _)) => compare(x, y);
        List.sort(cmp_fst)
    };

    test("can find great-grandparent", () => {
        let query = And([
            parent(Var("Parent"), atom("kylo")) -> Term,
            parent(Var("Grandparent"), Var("Parent")) -> Term,
            parent(Var("GreatGrandparent"), Var("Grandparent")) -> Term,
        ]);

        let ans = {
            [
                sort([
                    ("Parent", atom("leah")),
                    ("Grandparent", atom("anakin")),
                    ("GreatGrandparent", atom("shmi")),
                ])
            ]
        };

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });

    test("can find all grandparents of kylo", () => {
        let query = And([
            parent(Var("Parent"), atom("kylo")) -> Term,
            parent(Var("Grandparent"), Var("Parent")) -> Term,
        ]);

        let ans = [
            sort([
                ("Parent", atom("leah")),
                ("Grandparent", atom("anakin")),
            ]),
            sort([
                ("Parent", atom("leah")),
                ("Grandparent", atom("padme")),
            ])
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });

    test("can find every parent of kylo who is not the child of anakin", () => {
        let query = And([
            parent(Var("Parent"), atom("kylo")) -> Term,
            Not(parent(atom("anakin"), Var("Parent")) -> Term)
        ]);

        let ans = [
            [
                ("Parent", atom("han"))
            ]
        ];

        expect(kb |> solve_query(query) |> Seq.to_list) |> toEqual(ans)
    });

    test("will throw an error when a `not` defines a variable", () => {
        let query = And([
            Not(parent(atom("anakin"), Var("Parent")) -> Term),
            parent(Var("Parent"), atom("kylo")) -> Term,
        ]);

        let msg = "Variables must be bound BEFORE appearing in a `not` predicate!";
        expect(() => kb |> solve_query(query)) |> toThrowException(Invalid_query(msg))
    });
});