open Jest;
open Expect;
open! Expect.Operators;

open Query;
open UnifierSet;

describe("Expect", () => {
    let kb: knowledge_base = [
        Pred("likes", [atom("john"), atom("jane")]),
        Pred("likes", [atom("jane"), atom("carrey")]),
        Pred("likes", [atom("ted"), atom("matt")]),
        Pred("likes", [atom("jane"), atom("ted")]),
    ];

    test("no substitutions for an empty query", () => {
        let query = Query([]);
        expect(kb |> solve_query(query)) |> toEqual([[]])
    });

    test("one answer for simple query", () => {
        let kb = [
            Pred("likes", [atom("ted"), atom("matt")]),
        ];

        let query = Query([
            Pred("likes", [atom("ted"), Var("X")])
        ]);

        let expected = [
            [("X", atom("matt"))]
        ];

        expect(kb |> solve_query(query)) |> toEqual(expected)
    });

    test("two-part, disconnected query to work", () => {
        Js.log("=======DISCONNECTED=======");
        let query = Query([
            Pred("likes", [atom("ted"), Var("X")]),
            Pred("likes", [Var("Y"), atom("carrey")])
        ]);

        let ans = [
            [
                ("X", atom("matt")),
                ("Y", atom("jane")),
            ]
        ];

        // expect(kb |> solve_query(query)) |> toEqual(ans)
        let res = kb |> solve_query(query);
        Js.log("ACTUAL: " ++ string_of_var_mapping_list(res));
        Js.log("EXPECTED: " ++ string_of_var_mapping_list(ans));
        expect(res) |> toEqual(ans)
    });

    test("two-part, connected query to work", () => {
        Js.log("=======CONNECTED=======");
        let query = Query([
            Pred("likes", [Var("X"), Var("Y")]),
            Pred("likes", [Var("Y"), atom("ted")])
        ]);

        let ans = [
            [
                ("X", atom("john")),
                ("Y", atom("jane")),
            ]
        ];

        let res = kb |> solve_query(query);
        Js.log("ACTUAL: " ++ string_of_var_mapping_list(res));
        Js.log("EXPECTED: " ++ string_of_var_mapping_list(ans));
        expect(res) |> toEqual(ans)
    });
});