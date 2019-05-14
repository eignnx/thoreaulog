const E = require("./JsExports.bs");

exports.KnowledgeBase = E.create_knowledge_base;

const Term = {
    Atom: E.atom,
    Pred: E.pred,
    Var: E.var_,
    toString: E.string_of_term,
};

exports.Term = Term;

const Query = {
    Term: E.term,
    Not: E.not__,
    And: E.and_,
    toString: E.string_of_query,
};

exports.Query = Query;

function objectFromEntries(pairs) {
    const map = {};
    for (const [key, val] of pairs) {
        map[key] = val;
    }
    return map;
}

class KnowledgeBase {
    constructor(facts) {
        this.kb = E.knowledge_base_of_array(facts || []);
        this.baseUnifs = E.register_facts(this.kb);
    }

    addFact(headName, atomNames) {
        const pred = Term.Pred(headName, atomNames.map(Term.Atom));
        this.kb = E.cons(pred, this.kb);
        this.baseUnifs = E.register_fact(pred, this.baseUnifs);
    }

    query(query) {
        const unifs = E.regiter_query(query, this.baseUnifs);
        this.answers = E.solve(query, unifs, this.kb);
        return this.answers;
    }

    answer() {
        if (!this.answers) return undefined;
        const res = E.sequence_next(this.answers);
        if (res) {
            const [ans, next] = res;
            this.answers = next;
            return objectFromEntries(ans);
        } else {
            this.answers = undefined;
            return undefined;
        }
    }

    allAnswers() {
        if (!this.answers) return undefined;
        let results = [];
        let x;
        while (x = this.answer()) {
            results.push(x);
        }
        return results;
    }
}

exports.KnowledgeBase = KnowledgeBase;