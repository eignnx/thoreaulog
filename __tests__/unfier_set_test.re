
open Jest;
open Expect;
open! Expect.Operators;

open Demo;

module Opt = Belt.Option;

describe("Expect singleton unifier set", () => {
  let foo = Atom("foo");
  let bar = Atom("bar");
  let u = UnifSet.empty |> UnifSet.add(foo, Root(1));

  test("to find its element", () => {
    expect(UnifSet.find(foo, u)) |> toEqual(Root(1))
  });

  test("to not find some other element", () => {
    expect(UnifSet.mem(bar, u)) |> toBe(false)
  });
});

describe("Expect", () => {
  let x = Var("x");
  let y = Var("y");
  let foo = Atom("foo");
  let bar = Atom("bar");
  let p1 = Pred("likes", [foo, x]);
  let p2 = Pred("likes", [y, bar]);
  let u = UnifSet.(
    empty
    |> add(x, Root(1))
    |> add(y, Root(1))
    |> add(foo, Root(1))
    |> add(bar, Root(1))
    |> add(p1, Root(1))
    |> add(p2, Root(1))
  );

  let (&>) = Belt.Option.flatMap;

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
});