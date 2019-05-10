open Jest;
open Expect;
open! Expect.Operators;

open UnifierSet;
open Utils;

module Opt = Belt.Option;

describe("Expect singleton unifier set", () => {
    let foo = Pred("foo", []);
    let bar = Pred("bar", []);
    let u = empty |> register(foo);

    test("to find its element", () => {
        expect(CompSet.find(foo, u)) |> toEqual(Root(1))
    });

    test("to not find some other element", () => {
        expect(CompSet.mem(bar, u)) |> toBe(false)
    });
});

describe("Expect", () => {
    let x = Var("x");
    let y = Var("y");
    let foo = Pred("foo", []);
    let bar = Pred("bar", []);
    let p1 = Pred("likes", [foo, x]);
    let p2 = Pred("likes", [y, bar]);
    let u = from_list([x, y, foo, bar, p1, p2]);

    test("`find_root` of a root is itself", () => {
        expect(find_root(x, u)) |> toEqual(x)
    });

    test("unifying variables puts them under same root", () => {
        let u = u |> unify(x, y) |> Opt.getExn;
        expect(find_root(x, u)) |> toEqual(find_root(y, u))
    });

    test("direct unification of two atoms to return None", () => {
        expect(u |> unify(foo, bar)) |> toEqual(None)
    });

    test("indirect unification of two atoms to return None", () => {
        expect(u |> unify(x, foo)
                &> unify(y, bar)
                &> unify(y, x)
        ) |> toEqual(None)
    });

    test("unification of two predicates works correctly", () => {
        let u = u |> unify(p1, p2) |> Opt.getExn;
        expect(
        (find_root(y, u), find_root(x, u))
        ) |> toEqual((foo, bar))
    });

    test("registration of compound component registers subcomponents", () => {
        let u = empty |> register(p1);
        expect(u) |> toEqual(from_list([p1, foo, x]))
    });

    test("two predicates of different arity don't unify", () => {
        let p1 = Pred("pred", [foo]);
        let p2 = Pred("pred", [foo, bar]);
        let u = from_list([p1, p2]);
        expect(u |> unify(p1, p2)) |> toEqual(None)
    });

    test("unification of two predicates works", () => {
        let fact = Pred("likes", [atom("ted"), atom("matt")]);
        let query = Pred("likes", [atom("ted"), Var("X")]);
        let u = from_list([fact, query]);
        let ans = u |> unify(atom("matt"), Var("X"));
        expect(u |> unify(fact, query)) |> toEqual(ans)
    });
});

describe("Expect concretization to work", () => {
    test("in simple case", () => {
        let jane = atom("jane");
        let unifs = from_list([jane]);
        expect(unifs |> concretize(jane)) |> toEqual(jane)
    });

    test("in complex case", () => {
        let jane = atom("jane");
        let john = atom("john");
        let p1 = Pred("likes", [Var("X"), jane]);

        let unifs = from_list([jane, john, p1])
            |> unify(Var("X"), john)
            |> Opt.getExn;
        
        let ans = Pred("likes", [john, jane]);
        expect(concretize(p1, unifs)) |> toEqual(ans)
    });
});