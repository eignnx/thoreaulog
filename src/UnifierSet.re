type term = Var(string) | Pred(string, list(term));

let rec string_of_term = c => {
  switch (c) {
  | Var(s) => String.capitalize(s)
  | Pred(head, args) => switch (args) {
    | [] => {j|$head|j}
    | _list =>
      let args = args |> List.map(string_of_term) |> String.concat(", ");
      {j|$head($args)|j}
    }
    
  }
};

let atom = name => Pred(name, []);

type node = Root(int) | Child(term)

let string_of_node = n => {
  switch n {
  | Root(c) => {j|Root($c)|j}
  | Child(p) =>
    let parent = string_of_term(p);
    {j|Child($parent)|j}
  }
};

module CompSet = Map.Make({
  type t = term;
  let compare = compare;
});

type unifier_set = CompSet.t(node);
let empty = CompSet.empty;
let singleton = CompSet.singleton;

let string_of_unifier_set: unifier_set => string
= unifs => {
  unifs
  |> CompSet.bindings
  |> List.map(((k, v)) => string_of_term(k) ++ ": " ++ string_of_node(v))
  |> List.map(s => "\t" ++ s)
  |> String.concat(",\n")
  |> str => "{\n" ++ str ++ "\n}"
};

let rec register = (x: term, set: unifier_set) => {
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
and register_subcomps = (x: term, set: unifier_set) => {
  switch (x) {
  | Pred(_, args) => register_all(args, set)
  | _ => set
  }
};

let from_list = list => empty |> register_all(list);

let rec find_root_data = (x: term, set: unifier_set) => {
  switch (CompSet.find(x, set)) {
  | Root(size) => (x, size)
  | Child(parent) => find_root_data(parent, set)
  }
};

let find_root = (x: term, set: unifier_set) => {
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
  
  // If the heads are different, no way to unify.
  | (Pred(head_a, _), Pred(head_b, _)) when head_a != head_b => None

  // If heads match, unification possible if all args unify pairwise.
  | (Pred(_, args_a), Pred(_, args_b)) => 
    let reduce = (set_opt, x, y) => Utils.(
      unify(x, y) <$> set_opt
    );

    // Could be left or right fold here, doesn't really matter.
    try (List.fold_left2(reduce, Some(set), args_a, args_b)) {
    | Invalid_argument(_) => None // Lists of different lengths.
    }
  }
};
