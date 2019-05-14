
open Jest;
open Expect;

open Seq;

let seq = () => Cons(1, () => Cons(2, () => Cons(3, () => Nil)));

test("Empty works", () => {
    expect(empty()) |> toEqual(Nil)
});

test("Conversion to list works correctly", () => {
    expect(to_list(seq)) |> toEqual([1, 2, 3])
});

test("Conversion from list to Seq, then back to list gives original list", () => {
    let list = [1, 2, 3, 4];
    expect(list |> of_list |> to_list) |> toEqual(list)
});

test("Return works", () => {
    let seq = return(123);
    expect(seq |> to_list) |> toEqual([123])
});

test("Mapping works", () => {
    expect(seq |> map(x => x * 2) |> to_list) |> toEqual([2, 4, 6])
});

test("Filter works", () => {
    expect(seq |> filter(x => x != 2) |> to_list) |> toEqual([1, 3])
});

test("Filter Map works", () => {
    let f = x => (x == 2) ? None : Some(x);
    expect(seq |> filter_map(f) |> to_list) |> toEqual([1, 3])
});

test("Append", () => {
    let seq1 = [1,2,3] |> of_list;
    let seq2 = [4,5,6] |> of_list;
    expect(append(seq1, seq2) |> to_list) |> toEqual([1,2,3,4,5,6])
});

test("Flat Map", () => {
    expect(seq |> flat_map(x => of_list([x, x])) |> to_list) |> toEqual([1,1,2,2,3,3])
});

test("Fold Left", () => {
    expect(seq |> fold_left((+), 0)) |> toBe(6)
});

test("Iter", () => {
    let total = ref(0);
    seq |> iter(x => total := total^ + x);
    expect(total^) |> toBe(6)
});