open Jest;
open Expect;
open! Expect.Operators;

open UnifierSet;
open Utils;

module Opt = Belt.Option;

describe("Expect singleton unifier set", () => {
  let foo = Atom("foo");
  let bar = Atom("bar");
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
  let foo = Atom("foo");
  let bar = Atom("bar");
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
});