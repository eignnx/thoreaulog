const E = require("./JsExports.bs");

exports.KnowledgeBase = E.create_knowledge_base;

exports.Term = {
    Atom: E.atom,
    Pred: E.pred,
    Var: E.var_,
    toString: E.string_of_term,
};

exports.Query = {
    Term: E.term,
    Not: E.not__,
    And: E.and_,
    toString: E.string_of_query,
};

class KnowledgeBase {
    constructor(facts) {
        this.kb = E.knowledge_base_of_array(facts);
        this.baseUnifs = E.register_facts(this.kb);
    }

    query(query) {
        const unifs = E.regiter_query(query, this.baseUnifs);
        const results = E.solve(query, unifs, this.kb);
        return results.map(soln_set => {
            const map = {};
            for (const [key, val] of soln_set) {
                map[key] = val;
            }
            return map;
        });
    }
}

exports.KnowledgeBase = KnowledgeBase;