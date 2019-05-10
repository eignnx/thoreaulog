open Jest;
open Expect;
open! Expect.Operators;

open Utils;

describe("`flat_map` should", () => {

    test("return the empty list when given an empty list", () => {
        expect([] |> flat_map(x => x)) |> toEqual([])
    });

    test("correctly condense a list of lists", () => {
        let l = [[1, 2], [], [3]];
        expect(l |> flat_map(x => x)) |> toEqual([1, 2, 3])
    });

    test("correctly condense a list of lists", () => {
        let l = [1,2,3];
        let f = (x: int) => switch (x) {
        | 1 => [10]
        | 2 => [20, 20]
        | _ => []
        };
        expect(l |> flat_map(f)) |> toEqual([10, 20, 20])
    });
});