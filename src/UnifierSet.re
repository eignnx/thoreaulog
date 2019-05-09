type comp = Var(string) | Atom(string) | Pred(string, list(comp));

let string_of_comp = c => {
  switch c {
  | Var(s) => {j|Var($s)|j}
  | Atom(s) => {j|Atom($s)|j}
  | Pred(s, l) => {j|Pred($s, $l)|j}
  }
};

type node = Root(int) | Child(comp)

let string_of_node = n => {
  switch n {
  | Root(c) => {j|Root($c)|j}
  | Child(p) => {j|Child($p)|j}
  }
};

module CompSet = Map.Make({
  type t = comp;
  let compare = compare;
});

type unifier_set = CompSet.t(node);
let empty = CompSet.empty;
let singleton = CompSet.singleton;

// let string_of_unifier_set = u => {
//   let roots = UnifSet.filter((k, v) => switch(v) {
//     | Root(_) => true
//     | _ => false
//   });
//   u |> String.concat("\n")
// };

let rec register = (x: comp, set: unifier_set) => {
  open CompSet;
  if (!mem(x, set)) {
    set |> add(x, Root(1))
        |> register_subcomps(x)
   } else {
    // If `x` is already in the set, do nothing. TODO: Best approach?
    set
  }
}
and register_all = (list, set) => {
  open List;
  open Utils;
  fold_left(flip(register), set, list)
}
and register_subcomps = (x: comp, set: unifier_set) => {
  switch (x) {
  | Pred(_, args) => register_all(args, set)
  | _ => set
  }
};

let from_list = list => empty |> register_all(list);

let rec find_root_data = (x: comp, set: unifier_set) => {
  switch (CompSet.find(x, set)) {
  | Root(size) => (x, size)
  | Child(parent) => find_root_data(parent, set)
  }
};

let find_root = (x: comp, set: unifier_set) => {
  let (x, _size) = find_root_data(x, set);
  x
};

let size_of_root = (x, set) => {
  let (_x, size) = find_root_data(x, set);
  size
};

// Unifies `x` and `y` in the set `set`, returns a new set if unification
// succeeds, and `None` if it fails.
// Note: `x`, `y` and all of `x` and `y`'s subcomponents must be members of the
// unifer set before this fn is called.
let rec unify = (x, y, set) => {
  let (rx, sz_x) = find_root_data(x, set);
  let (ry, sz_y) = find_root_data(y, set);

  let sum_sz = sz_x + sz_y;
  let (small_root, large_root) = (sz_x < sz_y) ? (rx, ry) : (ry, rx);

  switch (small_root, large_root) {

  // When unifying two Vars, make the one with fewer children the child of
  // the other. This will keep the tree relatively shallow.
  | (Var(_), Var(_)) =>
    set
      -> CompSet.add(large_root, Root(sum_sz), _)
      -> CompSet.add(small_root, Child(large_root), _)
      -> Some

  // If a Var and a non-Var are unified, make the non-Var the root.
  | (Var(name), comp) | (comp, Var(name)) =>
    set
      -> CompSet.add(comp, Root(sum_sz), _)
      -> CompSet.add(Var(name), Child(comp), _)
      -> Some
  
  // Two atoms unify if they are called the same thing.
  | (Atom(a), Atom(b)) when a == b => Some(set)
  | (Atom(_), Atom(_)) => None

  // If the heads are different, no way to unify.
  | (Pred(head_a, _), Pred(head_b, _)) when head_a != head_b => None

  // If heads match, unification possible if all args unify pairwise.
  | (Pred(_, args_a), Pred(_, args_b)) => {
    let (<$>) = (f, x) => Belt.Option.flatMap(x, f);
    let reduce = (set_opt, x, y) => unify(x, y) <$> set_opt;

    // Could be left or right fold here, doesn't really matter.
    try (List.fold_left2(reduce, Some(set), args_a, args_b)) {
    | Invalid_argument(_) => None // Lists of different lengths.
    }
  }

  // In all other cases, no way to unify.
  | _ => None
  }
};
