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
        let query = Query([]);
        expect(kb |> solve_query(query)) |> toEqual([[]])
    });

    test("one answer for simple query", () => {
        let kb = [
            likes(atom("ted"), atom("matt")),
        ];

        let query = Query([
            likes(atom("ted"), Var("X"))
        ]);

        let expected = [
            [("X", atom("matt"))]
        ];

        expect(kb |> solve_query(query)) |> toEqual(expected)
    });

    test("two-part, disconnected query to work", () => {
        let query = Query([
            likes(atom("ted"), Var("X")),
            likes(Var("Y"), atom("carrey"))
        ]);

        let ans = [
            [
                ("X", atom("matt")),
                ("Y", atom("jane")),
            ]
        ];

        expect(kb |> solve_query(query)) |> toEqual(ans)
    });

    test("two-part, connected query to work", () => {
        let query = Query([
            likes(Var("X"), Var("Y")),
            likes(Var("Y"), atom("ted")),
        ]);

        let ans = [
            [
                ("X", atom("john")),
                ("Y", atom("jane")),
            ]
        ];

        expect(kb |> solve_query(query)) |> toEqual(ans)
    });
});

describe("Expect to find multiple solutions", () => {
    test("in simple case", () => {
        let query = Query([
            likes(atom("jane"), Var("X")),
        ]);
        
        let ans = [
            [
                ("X", atom("carrey"))
            ],
            [
                ("X", atom("ted"))
            ]
        ];

        expect(kb |> solve_query(query)) |> toEqual(ans)
    });
});

describe("Expect a long string of subqueries", () => {

    let long_query = [
        likes(atom("john"), Var("A")),  // A = jane
        likes(Var("A"), Var("B")),      // B = ted
        likes(Var("B"), atom("matt")),
    ];

    test("to work", () => {
        let query = Query(long_query);

        let ans = [
            [
                ("A", atom("jane")),
                ("B", atom("ted")),
            ]
        ];

        expect(kb |> solve_query(query)) |> toEqual(ans)
    });

    test("with extraneous fact to work", () => {
        let query = Query([
            likes(Var("A"), atom("carrey")), // Extraneous fact!
            ...long_query
        ]);

        let ans = [
            [
                ("A", atom("jane")),
                ("B", atom("ted")),
            ]
        ];

        expect(kb |> solve_query(query)) |> toEqual(ans)
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
        let query = Query([
            parent(Var("Parent"), atom("kylo")),
            parent(Var("Grandparent"), Var("Parent")),
            parent(Var("GreatGrandparent"), Var("Grandparent")),
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

        expect(kb |> solve_query(query)) |> toEqual(ans)
    });

    test("can find all grandparents of kylo", () => {
        let query = Query([
            parent(Var("Parent"), atom("kylo")),
            parent(Var("Grandparent"), Var("Parent")),
        ]);

        let ans = {
            [
                sort([
                    ("Parent", atom("leah")),
                    ("Grandparent", atom("anakin")),
                ]),
                sort([
                    ("Parent", atom("leah")),
                    ("Grandparent", atom("padme")),
                ])
            ]
        };

        expect(kb |> solve_query(query)) |> toEqual(ans)
    });
});