open Jest;
open Expect;
open! Expect.Operators;

open Query;
open UnifierSet;

describe("Expect", () => {
    let kb: list(term) = [
        Pred("likes", [atom("john"), atom("jane")]),
        Pred("likes", [atom("jane"), atom("carrey")]),
        Pred("likes", [atom("ted"), atom("matt")]),
        Pred("likes", [atom("jane"), atom("ted")]),
    ];

    test("no substitutions for an empty query", () => {
        let query = [];
        expect(kb |> solve_query(query)) |> toEqual([])
    });

    test("one answer for simple query", () => {
        let kb = [
            Pred("likes", [atom("ted"), atom("matt")]),
        ];

        let query = [
            Pred("likes", [atom("ted"), Var("X")])
        ];

        let expected = [
            [("X", atom("matt"))]
        ];

        expect(kb |> solve_query(query)) |> toEqual(expected)
    });
});