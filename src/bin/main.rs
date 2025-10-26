
use lib::core::types::type_substitution::TypeSubstitution;
use lib::utils::pretty_printer::*;
use lib::core::types::{type_variable::*, algebra_type::*, arrow_type::*, star_type::*};
use lib::core::terms::{minlog_term::*, term_variable::*, constructor::*, program_term::*, abstraction::*, application::*,
    term_substitution::*, tuple::*, projection::*};
use lib::core::predicates::{minlog_predicate::*, predicate_variable::*, inductive_predicate::*, predicate_substitution::*};
use lib::core::formulas::{prime_formula::*, implication::*, all_quantifier::*};

use lib::core::structures::{algebra::*, program_constant::*, inductive_constant::*};
use lib::core::proofs::{
    assumption::Assumption,
    implication_elim::ImplicationElim,
    implication_intro::ImplicationIntro,
    universal_elim::UniversalElim,
    universal_intro::UniversalIntro,
    theorem::Theorem,
};
use lib::utils::proof_tree_display::ProofTreeDisplayable;

fn main() {
    let t = TypeVariable::create("T".to_string());
    let a = TermVariable::create("a".to_string(), t.clone(), Totality::Total);

    let p = PredicateVariable::create("P".to_string(), vec![t.clone()], PredicateDegree { positive_content: true, negative_content: true });
    let q = PredicateVariable::create("Q".to_string(), vec![t.clone()], PredicateDegree { positive_content: true, negative_content: true });

    let p_a = PrimeFormula::create(p.clone(), vec![a.clone()]);
    let q_a = PrimeFormula::create(q.clone(), vec![a.clone()]);

    let imp_pa_qa = Implication::create(vec![p_a.clone()], q_a.clone());

    let all_impl = AllQuantifier::create(vec![a.clone()], imp_pa_qa.clone());
    
    let all_p = AllQuantifier::create(vec![a.clone()], p_a.clone());

    println!("\n=== Term and Type System Test Output ===\n");
    
    let type_var_1 = TypeVariable::create("T".to_string());
    let type_var_2 = TypeVariable::create("U".to_string());
    
    let star = StarType::create(vec![type_var_2.clone(); 3]);
    let arrow = ArrowType::create(vec![type_var_1.clone(); 2], type_var_1.clone());
    
    println!("Type Variable 1:");
    println!("{}", type_var_1.debug_string());
    println!("Type Variable 2:");
    println!("{}", type_var_2.debug_string());
    println!("Star Type:");
    println!("{}", star.debug_string());
    println!("Arrow Type:");
    println!("{}", arrow.debug_string());
    
    let var_x = TermVariable::create("x".to_string(), type_var_1.clone(), Totality::Total);
    let var_y = TermVariable::create("y".to_string(), type_var_2.clone(), Totality::Total);
    let var_z = TermVariable::create("z".to_string(), type_var_1.clone(), Totality::Total);
    let var_w = TermVariable::create("w".to_string(), type_var_2.clone(), Totality::Total);
    
    let abs = Abstraction::create(vec![var_x.clone(), var_y.clone()], var_x.clone());
    let app = Application::create(abs.clone(), vec![var_z.clone()]);
    
    let tuple = Tuple::create(vec![var_y.clone(), var_w.clone(), var_x.clone()]);
    let proj = Projection::create(tuple.clone(), 1);
    
    println!("Term Variable x:");
    println!("{}", var_x.debug_string());
    println!("Term Variable y:");
    println!("{}", var_y.debug_string());
    println!("Term Variable z:");
    println!("{}", var_z.debug_string());
    println!("Term Variable w:");
    println!("{}", var_w.debug_string());
    
    println!("Abstraction:");
    println!("{}", abs.debug_string());
    println!("Application:");
    println!("{}", app.debug_string());
    println!("Tuple:");
    println!("{}", tuple.debug_string());
    println!("Projection:");
    println!("{}", proj.debug_string());
    
    let normalized_app = app.normalize(true, true);
    println!("Normalized Application:");
    println!("{}", normalized_app.debug_string());
    
    let normalized_proj = proj.normalize(true, true);
    println!("Normalized Projection:");
    println!("{}", normalized_proj.debug_string());
    
    let subst = TermSubstitution::from_pairs(vec![
        (TermSubstEntry::Type(type_var_2.clone()), TermSubstEntry::Type(arrow.clone())),
    ]);
    
    println!("Substitution:");
    println!("{}", subst.debug_string());
    
    let substituted_app = subst.substitute(&TermSubstEntry::Term(app.clone())).to_term().unwrap();
    println!("Substituted Application:");
    println!("{}", substituted_app.debug_string());
    println!("Substituted Application Type:");
    println!("{}", substituted_app.minlog_type().debug_string());
    
    let substituted_normalized_app = subst.substitute(&TermSubstEntry::Term(normalized_app.clone())).to_term().unwrap();
    println!("Substituted Normalized Application:");
    println!("{}", substituted_normalized_app.debug_string());
    println!("Substituted Normalized Application Type:");
    println!("{}", substituted_normalized_app.minlog_type().debug_string());
    
    let nat = Algebra::create("Nat".to_string());
    let nat_type = AlgebraType::create(nat.clone(), TypeSubstitution::make_empty());
    println!("Algebra Type:");
    println!("{}", nat_type.debug_string());
    
    let zero = Constructor::create("Zero".to_string(), nat_type.clone());
    nat.add_constructor(zero);
    
    let succ_type = ArrowType::create(vec![nat_type.clone()], nat_type.clone());
    let succ = Constructor::create("Succ".to_string(), succ_type.clone());
    nat.add_constructor(succ);
    
    nat_type.to_algebra().unwrap().ensure_well_founded();

    println!("Algebra:");
    println!("{}", nat.debug_string());
    
    let list = Algebra::create("List".to_string());
    let list_type = AlgebraType::create(list.clone(), TypeSubstitution::make_empty());
    println!("Algebra Type:");
    println!("{}", list_type.debug_string());
    
    let nil = Constructor::create("Nil".to_string(), list_type.clone());
    list.add_constructor(nil);
    
    let cons_type = ArrowType::create(vec![type_var_1.clone(), list_type.clone()], list_type.clone());
    let cons = Constructor::create("Cons".to_string(), cons_type.clone());
    list.add_constructor(cons);
    
    list_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("Algebra:");
    println!("{}", list.debug_string());

    let nat_list_type = AlgebraType::create(list.clone(), TypeSubstitution::from_pairs(vec![
        (type_var_1.clone(), nat_type.clone())
    ]));
    println!("Nat List Type:");
    println!("{}", nat_list_type.debug_string());
    
    for constructor in nat_list_type.to_algebra().unwrap().constructors() {
        println!("Constructor:");
        println!("{}", constructor.debug_string());
        println!("Constructor Type:");
        println!("{}", constructor.minlog_type().debug_string());
    }
    
    let nat_zero = nat_type.to_algebra().unwrap().constructor(&"Zero".to_string()).unwrap();
    let nat_succ = nat_type.to_algebra().unwrap().constructor(&"Succ".to_string()).unwrap();
    
    let nat_0 = nat_zero.clone();
    let nat_1 = Application::create(nat_succ.clone(), vec![nat_0.clone()]);
    let nat_2 = Application::create(nat_succ.clone(), vec![nat_1.clone()]);
    
    println!("Natural Number 0:");
    println!("{}", nat_0.debug_string());
    println!("Natural Number 1:");
    println!("{}", nat_1.debug_string());
    println!("Natural Number 2:");
    println!("{}", nat_2.debug_string());
    
    let nat_list_nil = nat_list_type.to_algebra().unwrap().constructor(&"Nil".to_string()).unwrap();
    let nat_list_cons = nat_list_type.to_algebra().unwrap().constructor(&"Cons".to_string()).unwrap();
    
    let nat_list_0 = nat_list_nil.clone();
    let nat_list_1 = Application::create(nat_list_cons.clone(), vec![nat_0.clone(), nat_list_0.clone()]);
    let nat_list_2 = Application::create(nat_list_cons.clone(), vec![nat_1.clone(), nat_list_1.clone()]);
    let nat_list_3 = Application::create(nat_list_cons.clone(), vec![nat_2.clone(), nat_list_2.clone()]);
    
    println!("Natural Number List 0:");
    println!("{}", nat_list_0.debug_string());
    println!("Natural Number List 1:");
    println!("{}", nat_list_1.debug_string());
    println!("Natural Number List 2:");
    println!("{}", nat_list_2.debug_string());
    println!("Natural Number List 3:");
    println!("{}", nat_list_3.debug_string());
    
    let nat_add_type = ArrowType::create(vec![nat_type.clone(), nat_type.clone()], nat_type.clone());
    let nat_add = ProgramConstant::create("NatAdd".to_string(), nat_add_type.clone(), Totality::Total);
    
    let nat_var_0 = TermVariable::create("m".to_string(), nat_type.clone(), Totality::Total);
    let nat_var_1 = TermVariable::create("n".to_string(), nat_type.clone(), Totality::Total);
    
    let nat_add_term = ProgramTerm::create(nat_add.clone(), TermSubstitution::make_empty());
    println!("Program Term nat_add:");
    println!("{}", nat_add_term.debug_string());
    
    let nat_add_rule_0 = RewriteRule::create(
        Application::create(nat_add_term.clone(), vec![nat_0.clone(), nat_var_1.clone()]),
        nat_var_1.clone()
    );
    
    let nat_add_rule_succ = RewriteRule::create(
        Application::create(nat_add_term.clone(), vec![
            Application::create(nat_succ.clone(), vec![nat_var_1.clone()]),
            nat_var_0.clone()
        ]),
        Application::create(nat_succ.clone(), vec![
            Application::create(nat_add_term.clone(), vec![nat_var_1.clone(), nat_var_0.clone()])
        ])
    );
    
    nat_add.add_computation_rule(nat_add_rule_0);
    nat_add.add_computation_rule(nat_add_rule_succ);
    
    println!("Program Constant nat_add:");
    println!("{}", nat_add.debug_string());
    
    let mut num_1 = nat_0.clone();
    for _ in 0..3 {
        num_1 = Application::create(nat_succ.clone(), vec![num_1]);
    }
    
    let mut num_2 = nat_0.clone();
    for _ in 0..2 {
        num_2 = Application::create(nat_succ.clone(), vec![num_2]);
    }
    
    let addition = Application::create(nat_add_term.clone(), vec![num_1.clone(), num_2.clone()]);
    println!("Term addition:");
    println!("{}", addition.debug_string());
    
    let normalized_add = addition.normalize(true, true);
    println!("Normalized Term addition:");
    println!("{}", normalized_add.debug_string());
    
    let list_concat_type = ArrowType::create(vec![list_type.clone(), list_type.clone()], list_type.clone());
    let list_concat = ProgramConstant::create("ListConcat".to_string(),
        list_concat_type.clone(), Totality::Total);
    
    let list_var_0 = TermVariable::create("l1".to_string(), list_type.clone(), Totality::Total);
    let list_var_1 = TermVariable::create("l2".to_string(), list_type.clone(), Totality::Total);
    
    let list_concat_term = ProgramTerm::create(list_concat.clone(), TermSubstitution::make_empty());
    println!("Program Term list_concat:");
    println!("{}", list_concat_term.debug_string());
    println!("Program Term list_concat Type:");
    println!("{}", list_concat_term.minlog_type().debug_string());
    
    let nil = list_type.to_algebra().unwrap().constructor(&"Nil".to_string()).unwrap();
    let cons = list_type.to_algebra().unwrap().constructor(&"Cons".to_string()).unwrap();
    
    let list_concat_rule_0 = RewriteRule::create(
        Application::create(list_concat_term.clone(), vec![nil.clone(), list_var_1.clone()]),
        list_var_1.clone()
    );
    
    let list_concat_rule_cons = RewriteRule::create(
        Application::create(list_concat_term.clone(), vec![
            Application::create(cons.clone(), vec![var_x.clone(), list_var_0.clone()]),
            list_var_1.clone()
        ]),
        Application::create(cons.clone(), vec![
            var_x.clone(),
            Application::create(list_concat_term.clone(), vec![list_var_0.clone(), list_var_1.clone()])
        ])
    );
    
    list_concat.add_computation_rule(list_concat_rule_0);
    list_concat.add_computation_rule(list_concat_rule_cons);
    
    println!("Program Constant list_concat:");
    println!("{}", list_concat.debug_string());

    let nat_list_concat = ProgramTerm::create(list_concat.clone(), TermSubstitution::from_pairs(vec![
        (type_var_1.clone().into(), nat_type.clone().into())
    ]));
    println!("Program Term nat_list_concat:");
    println!("{}", nat_list_concat.debug_string());
    
    let nat_list_concat = Application::create(nat_list_concat.clone(), vec![nat_list_2.clone(), nat_list_3.clone()]);
    println!("Term nat_list_concat:");
    println!("{}", nat_list_concat.debug_string());
    
    let normalized_nat_list_concat = nat_list_concat.normalize(true, true);
    println!("Normalized Term nat_list_concat:");
    println!("{}", normalized_nat_list_concat.debug_string());
    
    let pvar = PredicateVariable::create("P".to_string(), vec![type_var_1.clone()],
        PredicateDegree { positive_content: true, negative_content: true });
    println!("Predicate Variable P:");
    println!("{}", pvar.debug_string());
    
    let exd_const = InductiveConstant::create("ExD".to_string(), vec![type_var_1.clone()]);
    let exd_const_type = InductivePredicate::create(exd_const.clone(), PredicateSubstitution::make_empty());
    
    println!("Inductive Predicate ExD:");
    println!("{}", exd_const_type.debug_string());
    
    let exd_clause = AllQuantifier::create(
        vec![var_x.clone()],
        Implication::create(
            vec![PrimeFormula::create(
                pvar.clone(),
                vec![var_x.clone()]
            )],
            PrimeFormula::create(
                exd_const_type.clone(),
                vec![var_x.clone()]
            )
        )
    );
    
    exd_const.add_clause("InitExD".to_string(), exd_clause);
    exd_const_type.to_inductive_predicate().unwrap().ensure_well_founded();
    
    println!("Inductive Constant ExD:");
    println!("{}", exd_const.debug_string());
    
    let yprod_alg = Algebra::create("YProd".to_string());
    let yprod_type = AlgebraType::create(yprod_alg.clone(), TypeSubstitution::make_empty());
    
    println!("Algebra Type YProd:");
    println!("{}", yprod_type.debug_string());
    
    exd_const.make_computational(yprod_alg, false);
    println!("Inductive Constant ExD after making computational:");
    println!("{}", exd_const.debug_string());
    
    println!("YProd Algebra:");
    println!("{}", yprod_type.to_algebra().unwrap().algebra().debug_string());
    
    let exd_cterm = MinlogPredicate::to_cterm(&exd_const_type);
    println!("Comprehension Term for ExD:");
    println!("{}", exd_cterm.debug_string());
    
    let nat_eq = InductiveConstant::create("NatEq".to_string(), vec![nat_type.clone(), nat_type.clone()]);
    let nat_eq_type = InductivePredicate::create(nat_eq.clone(), PredicateSubstitution::make_empty());
    
    println!("Inductive Predicate NatEq:");
    println!("{}", nat_eq_type.debug_string());
    
    let nat_eq_zero_clause = PrimeFormula::create(
        nat_eq_type.clone(),
        vec![nat_0.clone(), nat_0.clone()]
    );
    nat_eq.add_clause("NatEqZero".to_string(), nat_eq_zero_clause);
    
    let nat_eq_succ_clause = AllQuantifier::create(
        vec![nat_var_0.clone(), nat_var_1.clone()],
        Implication::create(
            vec![PrimeFormula::create(
                nat_eq_type.clone(),
                vec![nat_var_0.clone(), nat_var_1.clone()]
            )],
            PrimeFormula::create(
                nat_eq_type.clone(),
                vec![
                    Application::create(nat_succ.clone(), vec![nat_var_0.clone()]),
                    Application::create(nat_succ.clone(), vec![nat_var_1.clone()])
                ]
            )
        )
    );
    nat_eq.add_clause("NatEqSucc".to_string(), nat_eq_succ_clause);
    
    nat_eq_type.to_inductive_predicate().unwrap().ensure_well_founded();
    
    println!("Inductive Constant NatEq:");
    println!("{}", nat_eq.debug_string());
    
    let nat_eq_alg = Algebra::create("NatEqAlg".to_string());
    nat_eq.make_computational(nat_eq_alg, false);
    
    println!("Inductive Constant NatEq after making computational:");
    println!("{}", nat_eq.debug_string());
    
    println!("NatEqAlg Algebra:");
    println!("{}", nat_eq_type.to_inductive_predicate().unwrap().get_algebra().unwrap().to_algebra().unwrap().algebra().debug_string());
    
    let nat_eq_cterm = MinlogPredicate::to_cterm(&nat_eq_type);
    println!("Comprehension Term for NatEq:");
    println!("{}", nat_eq_cterm.debug_string());

    // --- Construct the formal Minlog proof for
    // (∀x. (P(x) → Q(x))) → (∀x. P(x) → ∀x. Q(x))
    {
        let asm1 = Assumption::create("u1".to_string(), all_impl.clone());
        let asm2 = Assumption::create("u2".to_string(), all_p.clone());

        let p_a_pf = UniversalElim::create(asm2.clone(), a.clone());
        let imp_pa_qa_pf = UniversalElim::create(asm1.clone(), a.clone());
        let q_a_pf = ImplicationElim::create(imp_pa_qa_pf.clone(), p_a_pf.clone());
        let all_q_pf = UniversalIntro::create(q_a_pf.clone(), a.clone());
        let imp_allp_allq_pf = ImplicationIntro::create(all_q_pf.clone(), asm2.clone());
        let full_pf = ImplicationIntro::create(imp_allp_allq_pf.clone(), asm1.clone());

        // Render and print the proof tree
        let mut proof_node = full_pf.to_proof_tree_node();
        proof_node.layout();
        println!("\n=== Formal Minlog proof tree ===\n");
        println!("{}", proof_node.render());

        // Optionally wrap as a named theorem and print
        let thm = Theorem::create("quantifier_transfer".to_string(), full_pf.clone());
        println!("\nNamed theorem:\n{}\n", thm.display_string());
    }
}