# Thoreaulog
A simple Prolog implementation written in ReasonML. Use it in your web applications!

## Example

```javascript
const kb = new KnowledgeBase();
kb.addFact("likes", ["darcy", "carrie"]);
kb.addFact("likes", ["carrie", "darcy"]);

const likes = (a, b) => Query.Term(Term.Pred("likes", [a, b]));
const q = Query.And([
    likes(Term.Var("X"), Term.Var("Y")),
    likes(Term.Var("Y"), Term.Var("X")),
]);

const ans = [
    {
        "X": Term.Atom("carrie"),
        "Y": Term.Atom("darcy"),
    },
    {
        "X": Term.Atom("darcy"),
        "Y": Term.Atom("carrie"),
    },
];

expect(kb.query(q)).toEqual(ans);
```

## Build
```
npm run build
```

## Run Tests
Requires `jest` is installed on your system (`$ which jest` should return something).
```
npm test
```
