# Thoreaulog
A simple Prolog implementation written in ReasonML. Use it in your web applications!

## Example

```javascript
const {
    KnowledgeBase,
    Query,
    Term,
} = require("thoreaulog");

const kb = new KnowledgeBase();
kb.addFact("likes", ["darcy", "carrie"]);
kb.addFact("likes", ["carrie", "darcy"]);

const likes = (a, b) => Query.Term(Term.Pred("likes", [a, b]));
const [X, Y] = [Term.Var("X"), Term.Var("Y")];

const q = Query.And([
    likes(X, Y), likes(Y, X)
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

// Extract answers incrementally...
kb.query(q);
const actual = [kb.answer(), kb.answer()];
expect(actual).toEqual(ans);

// ...or all at once.
kb.query(q);
expect(kb.allAnswers()).toEqual(ans);
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
