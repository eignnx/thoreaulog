# Thoreaulog
A simple Prolog implementation written in ReasonML. Use it in your web applications!

## Example

```javascript
describe("`KnowledgeBase` objects", () => {
    let likes = (a, b) => Term.Pred("likes", [Term.Atom(a), Term.Atom(b)]);

    const kb = new KnowledgeBase([
        likes("carrie", "darcy"),
        likes("darcy", "carrie"),
    ]);

    likes = (a, b) => Term.Pred("likes", [a, b])

    test("solve correctly", () => {
        const query = Query.And([
            likes(Term.Var("X"), Term.Var("Y")),
            likes(Term.Var("Y"), Term.Var("X")),
        ].map(Query.Term));

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

        expect(kb.query(query)).toEqual(ans);
    });
});
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
