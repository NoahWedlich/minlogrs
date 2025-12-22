
use indexmap::{IndexMap, IndexSet};
use std::rc::Rc;
use lib::{builtin::{elimination::extract_elimination_axiom, totality::extract_totality},
core::{predicates::comprehension_term::ComprehensionTerm,
    proofs::{minlog_proof::{MinlogProof, ProofBody},
    proof_context::ProofContext},
    structures::program_constant::{ProgramConstant, RewriteRule},
    terms::{application::Application, program_term::ProgramTerm, term_substitution::TermSubstitution}, types::type_constant::TypeConstant}};
use lib::proof_generation::by_use::generate_proof_by_use;
use lib::proof_generation::by_assume::{generate_proof_by_assume, generate_proof_by_assume_with_name};
use lib::proof_generation::by_intro::generate_proof_by_intro;
use lib::proof_generation::by_elim::generate_proof_by_elim;
use lib::core::predicates::all_quantifier::AllQuantifier;
use lib::core::predicates::implication::Implication;
use lib::core::predicates::predicate_substitution::PredicateSubstitution;
use lib::core::predicates::inductive_predicate::InductivePredicate;
use lib::core::predicates::prime_formula::PrimeFormula;
use lib::core::structures::inductive_constant::InductiveConstant;
use lib::core::terms::term_variable::TermVariable;
use lib::core::types::tuple_type::TupleType;
use lib::core::types::type_substitution::TypeSubstitution;
use lib::core::types::type_variable::TypeVariable;
use lib::utils::pretty_printer::*;
use lib::utils::proof_tree_display::*;
use lib::core::types::{algebra_type::*, arrow_type::*};
use lib::core::terms::constructor::*;

use lib::core::structures::{algebra::*};

fn main() {
    let tvar = TypeVariable::create("T".to_string());
    let tmvar = TermVariable::create("x".to_string(), tvar.clone());
    
    let eq_def = InductiveConstant::create("Eq".to_string(), TupleType::create(vec![tvar.clone(), tvar.clone()]));
    let eq_pred = InductivePredicate::create(eq_def.clone(), PredicateSubstitution::make_empty());
    
    println!("Equality Predicate:");
    println!("{}", eq_pred.debug_string());
    
    let eq_intro = AllQuantifier::create(
        vec![tmvar.clone()],
        PrimeFormula::create(
            eq_pred.clone(),
            vec![tmvar.clone(), tmvar.clone()]
        )
    );
    eq_def.add_clause("EqIntro".to_string(), eq_intro);
    
    eq_pred.to_inductive_predicate().unwrap().ensure_well_founded();
    
    let eq_alg = Algebra::create("EqAlg".to_string());
    let eq_alg_type = AlgebraType::create(eq_alg.clone(), TypeSubstitution::make_empty());
    eq_def.register_computational_content(eq_alg_type.clone());
    eq_def.make_computational(false);
    
    println!("Equality Constant:");
    println!("{}", eq_def.debug_string());
    
    let eq_elim = extract_elimination_axiom(&eq_pred, &mut IndexMap::new());
    println!("Equality Elimination Axiom:");
    println!("{}", eq_elim.render_proof_tree());
    
    let eq_elim_proof = generate_proof_by_elim(&eq_pred);
    println!("Equality Elimination Proof:");
    println!("{}", eq_elim_proof.render_proof_tree());
    
    println!("Equality Algebra:");
    println!("{}", eq_alg.debug_string());
    
    let bool = Algebra::create("Bool".to_string());
    let bool_type = AlgebraType::create(bool.clone(), TypeSubstitution::make_empty());
    println!("Algebra Type:");
    println!("{}", bool_type.debug_string());
    
    let true_const = Constructor::create("True".to_string(), bool_type.clone());
    bool.add_constructor(true_const.clone());
    
    let false_const = Constructor::create("False".to_string(), bool_type.clone());
    bool.add_constructor(false_const.clone());
    
    bool_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("Algebra:");
    println!("{}", bool.debug_string());
    
    let atom_def = InductiveConstant::create("Atom".to_string(), TupleType::create(vec![bool_type.clone()]));
    let atom_pred = InductivePredicate::create(atom_def.clone(), PredicateSubstitution::make_empty());
    
    println!("Atom Predicate:");
    println!("{}", atom_pred.debug_string());
    
    let bool_var = TermVariable::create("b".to_string(), bool_type.clone());
    let atom_intro = AllQuantifier::create(
        vec![bool_var.clone()],
        Implication::create(
            vec![
                PrimeFormula::create(
                    eq_pred.substitute(&tvar.clone().into(), &bool_type.clone().into()),
                    vec![bool_var.clone(), true_const.clone()]
                )
            ],
            PrimeFormula::create(
                atom_pred.clone(),
                vec![bool_var.clone()]
            )
        )
    );
    atom_def.add_clause("AtomIntro".to_string(), atom_intro);
    
    atom_pred.to_inductive_predicate().unwrap().ensure_well_founded();
    
    let atom_alg = Algebra::create("AtomAlg".to_string());
    let atom_alg_type = AlgebraType::create(atom_alg.clone(), TypeSubstitution::make_empty());
    atom_def.register_computational_content(atom_alg_type.clone());
    atom_def.make_computational(false);
    
    println!("Atom Constant:");
    println!("{}", atom_def.debug_string());
    
    let atom_elim = extract_elimination_axiom(&atom_pred, &mut IndexMap::new());
    println!("Atom Elimination Axiom:");
    println!("{}", atom_elim.render_proof_tree());
    
    let atom_elim_proof = generate_proof_by_elim(&atom_pred);
    println!("Atom Elimination Proof:");
    println!("{}", atom_elim_proof.render_proof_tree());
    
    println!("Atom Algebra:");
    println!("{}", atom_alg.debug_string());
    
    let nat = Algebra::create("Nat".to_string());
    let nat_type = AlgebraType::create(nat.clone(), TypeSubstitution::make_empty());
    println!("Algebra Type:");
    println!("{}", nat_type.debug_string());
    
    let zero = Constructor::create("Zero".to_string(), nat_type.clone());
    nat.add_constructor(zero.clone());
    
    let succ_type = ArrowType::create(vec![nat_type.clone()], nat_type.clone());
    let succ = Constructor::create("Succ".to_string(), succ_type.clone());
    nat.add_constructor(succ.clone());
    
    nat_type.to_algebra().unwrap().ensure_well_founded();

    println!("Algebra:");
    println!("{}", nat.debug_string());
    
    let mut totalities = IndexMap::new();
    
    let nat_total = extract_totality(&nat_type, &mut totalities);
    println!("Totality Predicate:");
    println!("{}", nat_total.to_inductive_predicate().unwrap().definition().debug_string());
    
    let nat_total_elim = extract_elimination_axiom(&nat_total, &mut IndexMap::new());
    println!("Totality Elimination Axiom:");
    println!("{}", nat_total_elim.render_proof_tree());
    
    let nat_total_elim_proof = generate_proof_by_elim(&nat_total);
    println!("Totality Elimination Proof:");
    println!("{}", nat_total_elim_proof.render_proof_tree());
    
    let list_alg = Algebra::create("List".to_string());
    let list_type = AlgebraType::create(list_alg.clone(), TypeSubstitution::make_empty());
    
    let nil_constr = Constructor::create("Nil".to_string(), list_type.clone());
    list_alg.add_constructor(nil_constr.clone());
    
    let cons_type = ArrowType::create(vec![tvar.clone(), list_type.clone()], list_type.clone());
    let cons_constr = Constructor::create("Cons".to_string(), cons_type.clone());
    list_alg.add_constructor(cons_constr.clone());
    
    list_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("List Algebra:");
    println!("{}", list_alg.debug_string());
    
    let nat_list = AlgebraType::create(list_alg.clone(), TypeSubstitution::from_pairs(
        vec![(tvar.clone(), nat_type.clone())]
    ));
    println!("Nat List Type:");
    println!("{}", nat_list.debug_string());
    
    let nat_list_total = extract_totality(&nat_list, &mut totalities);
    println!("Nat List Totality Predicate:");
    println!("{}", nat_list_total.to_inductive_predicate().unwrap().definition().debug_string());
    
    let nat_list_total_elim = extract_elimination_axiom(&nat_list_total, &mut IndexMap::new());
    println!("Nat List Totality Elimination Axiom:");
    println!("{}", nat_list_total_elim.render_proof_tree());
    
    let nat_list_total_elim_proof = generate_proof_by_elim(&nat_list_total);
    println!("Nat List Totality Elimination Proof:");
    println!("{}", nat_list_total_elim_proof.render_proof_tree());
    
    let tree_alg = Algebra::create("Tree".to_string());
    let tree_type = AlgebraType::create(tree_alg.clone(), TypeSubstitution::make_empty());
    
    let leaf_constr = Constructor::create("Leaf".to_string(), ArrowType::create(vec![tvar.clone()], tree_type.clone()));
    tree_alg.add_constructor(leaf_constr.clone());
    
    let node_type = ArrowType::create(vec![tvar.clone(), tree_type.clone(), tree_type.clone()], tree_type.clone());
    let node_constr = Constructor::create("Node".to_string(), node_type.clone());
    tree_alg.add_constructor(node_constr.clone());
    
    tree_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("Tree Algebra:");
    println!("{}", tree_alg.debug_string());
    
    let nat_tree = AlgebraType::create(tree_alg.clone(), TypeSubstitution::from_pairs(
        vec![(tvar.clone(), nat_type.clone())]
    ));
    
    println!("Nat Tree Type:");
    println!("{}", nat_tree.debug_string());
    
    let nat_tree_total = extract_totality(&nat_tree, &mut totalities);
    println!("Nat Tree Totality Predicate:");
    println!("{}", nat_tree_total.to_inductive_predicate().unwrap().definition().debug_string());
    
    let nat_tree_total_elim = extract_elimination_axiom(&nat_tree_total, &mut IndexMap::new());
    println!("Nat Tree Totality Elimination Axiom:");
    println!("{}", nat_tree_total_elim.render_proof_tree());
    
    let nat_tree_total_elim_proof = generate_proof_by_elim(&nat_tree_total);
    println!("Nat Tree Totality Elimination Proof:");
    println!("{}", nat_tree_total_elim_proof.render_proof_tree());
    
    let even_const = InductiveConstant::create("Even".to_string(), TupleType::create(vec![nat_type.clone()]));
    let even_pred = InductivePredicate::create(even_const.clone(), PredicateSubstitution::make_empty());
    
    let odd_const = InductiveConstant::create("Odd".to_string(), TupleType::create(vec![nat_type.clone()]));
    let odd_pred = InductivePredicate::create(odd_const.clone(), PredicateSubstitution::make_empty());
    
    let zero_even = PrimeFormula::create(even_pred.clone(), vec![zero.clone()]);
    even_const.add_clause("ZeroEven".to_string(), zero_even);
    
    let nat_var_1 = TermVariable::create("n".to_string(), nat_type.clone());
    let nat_var_2 = TermVariable::create("m".to_string(), nat_type.clone());
    let nat_var_3 = TermVariable::create("n0".to_string(), nat_type.clone());
    let nat_var_4 = TermVariable::create("m0".to_string(), nat_type.clone());
    
    let succ_even_odd = AllQuantifier::create(
        vec![nat_var_1.clone()],
        Implication::create(
            vec![
                PrimeFormula::create(
                    odd_pred.clone(),
                    vec![nat_var_1.clone()]
                )
            ],
            PrimeFormula::create(
                even_pred.clone(),
                vec![Application::create(succ.clone(), vec![nat_var_1.clone()])]
            )
        )
    );
    even_const.add_clause("SuccEvenOdd".to_string(), succ_even_odd);
    
    let succ_odd_even = AllQuantifier::create(
    vec![nat_var_2.clone()],
    Implication::create(
        vec![
            PrimeFormula::create(
                even_pred.clone(),
                vec![nat_var_2.clone()]
            )
            ],
            PrimeFormula::create(
                odd_pred.clone(),
                vec![Application::create(succ.clone(), vec![nat_var_2.clone()])]
            )
        )
    );
    odd_const.add_clause("SuccOddEven".to_string(), succ_odd_even);
    
    even_pred.to_inductive_predicate().unwrap().ensure_well_founded();
    odd_pred.to_inductive_predicate().unwrap().ensure_well_founded();
    
    println!("Even Predicate:");
    println!("{}", even_const.debug_string());
    
    println!("Odd Predicate:");
    println!("{}", odd_const.debug_string());
    
    let even_alg = Algebra::create("EvenAlg".to_string());
    let even_alg_type = AlgebraType::create(even_alg.clone(), TypeSubstitution::make_empty());
    
    let odd_alg = Algebra::create("OddAlg".to_string());
    let odd_alg_type = AlgebraType::create(odd_alg.clone(), TypeSubstitution::make_empty());
    
    even_const.register_computational_content(even_alg_type.clone());
    odd_const.register_computational_content(odd_alg_type.clone());
    
    even_const.make_computational(false);
    odd_const.make_computational(false);
    
    println!("Even Algebra:");
    println!("{}", even_alg.debug_string());
    
    println!("Odd Algebra:");
    println!("{}", odd_alg.debug_string());
    
    let even_elim = extract_elimination_axiom(&even_pred, &mut IndexMap::new());
    println!("Even Elimination Axiom:");
    println!("{}", even_elim.render_proof_tree());
    
    let odd_elim = extract_elimination_axiom(&odd_pred, &mut IndexMap::new());
    println!("Odd Elimination Axiom:");
    println!("{}", odd_elim.render_proof_tree());
    
    let nat_eq = ProgramConstant::create(
        "NatEq".to_string(),
        ArrowType::create(
            vec![nat_type.clone(), nat_type.clone()],
            bool_type.clone()
        ),
    );
    let nat_eq_term = ProgramTerm::create(nat_eq.clone(), TermSubstitution::make_empty());
    
    println!("Program Term:");
    println!("{}", nat_eq_term.debug_string());
    
    let zero_eq_zero = RewriteRule::create(
        Application::create(
            nat_eq_term.clone(),
            vec![zero.clone(), zero.clone()]
        ),
        true_const.clone()
    );
    nat_eq.add_computation_rule(zero_eq_zero);
    
    let zero_neq_succ = RewriteRule::create(
        Application::create(
            nat_eq_term.clone(),
            vec![zero.clone(), Application::create(succ.clone(), vec![nat_var_1.clone()])]
        ),
        false_const.clone()
    );
    nat_eq.add_computation_rule(zero_neq_succ);
    
    let succ_eq_succ = RewriteRule::create(
        Application::create(
            nat_eq_term.clone(),
            vec![
                Application::create(succ.clone(), vec![nat_var_1.clone()]),
                Application::create(succ.clone(), vec![nat_var_2.clone()])
            ]
        ),
        Application::create(
            nat_eq_term.clone(),
            vec![nat_var_1.clone(), nat_var_2.clone()]
        )
    );
    nat_eq.add_computation_rule(succ_eq_succ);
    
    println!("Program Constant:");
    println!("{}", nat_eq.debug_string());
    
    let algebra = Algebra::create("TestAlgebra".to_string());
    let algebra_type = AlgebraType::create(algebra.clone(), TypeSubstitution::make_empty());
    
    let test_const_type_0 = ArrowType::create(
        vec![tvar.clone()],
        algebra_type.clone()
    );
    let test_const_type_1 = ArrowType::create(
        vec![tvar.clone(), algebra_type.clone()],
        algebra_type.clone()
    );
    
    let test_const_0 = Constructor::create("TestConst0".to_string(), test_const_type_0.clone());
    algebra.add_constructor(test_const_0.clone());
    
    let test_const_1 = Constructor::create("TestConst1".to_string(), test_const_type_1.clone());
    algebra.add_constructor(test_const_1.clone());
    
    algebra_type.to_algebra().unwrap().ensure_well_founded();
    
    println!("Algebra:");
    println!("{}", algebra.debug_string());
    
    algebra.add_reduction(IndexSet::from([tvar.clone()]), nat.clone());
    
    let test_subst = TypeSubstitution::from_pairs(
        vec![(tvar.clone(), TypeConstant::create_null())]
    );
    let reduced_algebra_type = test_subst.substitute(&algebra_type).remove_nulls().unwrap();
    println!("Reduced Algebra Type:");
    println!("{}", reduced_algebra_type.debug_string());
    
    let target = AllQuantifier::create(
        vec![nat_var_1.clone(), nat_var_2.clone()],
        Implication::create(
            vec![
                PrimeFormula::create(
                    nat_total.clone(),
                    vec![nat_var_1.clone()]
                ),
                PrimeFormula::create(
                    nat_total.clone(),
                    vec![nat_var_2.clone()]
                ),
                PrimeFormula::create(
                eq_pred.substitute(&tvar.clone().into(), &nat_type.clone().into()),
                vec![nat_var_1.clone(), nat_var_2.clone()]
            )
            ],
            PrimeFormula::create(
                atom_pred.clone(),
                vec![Application::create(
                    nat_eq_term.clone(),
                    vec![nat_var_1.clone(), nat_var_2.clone()]
                )]
            )
        )
    );
    println!("Target Formula:");
    println!("{}", target.debug_string());
    
    let assume_proof = generate_proof_by_assume(
        &target,
        &vec!["n".to_string(), "m".to_string(), "Tn".to_string(), "Tm".to_string(), "eq_nm".to_string()],
        &ProofContext::new()
    );
    println!("Assume Proof:");
    println!("{}", assume_proof.render_proof_tree());
    
    let goal0 = assume_proof.get_goals().iter().next().unwrap()
        .to_goal().unwrap().clone();
    let context0 = goal0.get_context().clone();

    println!("Goal to Prove:");
    println!("{}", goal0.render_proof_tree());
    
    let subproof0 = generate_proof_by_intro(
        &goal0.proved_formula(),
        &"AtomIntro".to_string(),
        &context0
    );
    println!("Subproof by Introduction:");
    println!("{}", subproof0.render_proof_tree());
    
    let goal1 = subproof0.get_goals().iter().next().unwrap()
        .to_goal().unwrap().clone();
    let context1 = goal1.get_context().clone();
    
    println!("Next Goal to Prove:");
    println!("{}", goal1.render_proof_tree());
    
    let formula_to_prove0 = PrimeFormula::create(
        ComprehensionTerm::create(
            vec![nat_var_3.clone(), nat_var_4.clone()],
            Implication::create(
                vec![
                    PrimeFormula::create(
                        nat_total.clone(),
                        vec![nat_var_3.clone()]
                    ),
                    PrimeFormula::create(
                        nat_total.clone(),
                        vec![nat_var_4.clone()]
                    ),
                ],
                PrimeFormula::create(
                    eq_pred.substitute(&tvar.clone().into(), &bool_type.clone().into()),
                    vec![
                        Application::create(nat_eq_term.clone(), vec![nat_var_3.clone(), nat_var_4.clone()]),
                        true_const.clone()
                    ]
                )
            )
        ),
        vec![nat_var_1.clone(), nat_var_2.clone()]
    );
    
    println!("Next Goal Formula:");
    println!("{}", formula_to_prove0.debug_string());
    
    let subproof1 = generate_proof_by_use(
        &formula_to_prove0,
        &eq_elim,
        &context1
    );
    println!("Subproof by Use:");
    println!("{}", subproof1.render_proof_tree());
    
    let normalized_proof = subproof1.normalize(false, false);
    println!("Normalized Proof:");
    println!("{}", normalized_proof.render_proof_tree());
    
    let goal2 = normalized_proof.get_goals().iter().next().unwrap()
        .to_goal().unwrap().clone();
    let context2 = goal2.get_context().clone();
    
    println!("Next Goal to Prove:");
    println!("{}", goal2.render_proof_tree());
    
    let subproof2 = generate_proof_by_assume_with_name(
        &goal2.proved_formula(),
        &vec!["x".to_string(), "Tx".to_string()],
        &context2,
        "g2".to_string()
    );
    println!("Subproof by Assume:");
    println!("{}", subproof2.render_proof_tree());
    
    let goal3 = subproof2.get_goals().iter().next().unwrap()
        .to_goal().unwrap().clone();
    let context3 = goal3.get_context().clone();
    
    println!("Next Goal to Prove:");
    println!("{}", goal3.render_proof_tree());
    
    let formula_to_prove1 = Implication::create(
        vec![
            PrimeFormula::create(
                nat_total.clone(),
                vec![nat_var_1.clone()]
            ),
        ],
        PrimeFormula::create(
            ComprehensionTerm::create(
                vec![nat_var_3.clone()],
                PrimeFormula::create(
                    eq_pred.substitute(&tvar.clone().into(), &bool_type.clone().into()),
                    vec![
                        Application::create(
                            nat_eq_term.clone(),
                            vec![nat_var_3.clone(), nat_var_3.clone()]
                        ),
                        true_const.clone()
                    ]
                )
            ),
            vec![nat_var_1.clone()]
        )
    );
    println!("Next Goal Formula:");
    println!("{}", formula_to_prove1.debug_string());
    
    let subproof3 = generate_proof_by_use(
        &formula_to_prove1,
        &nat_total_elim_proof,
        &context3
    );
    println!("Subproof by Use:");
    println!("{}", subproof3.render_proof_tree());
    
    let normalized_proof2 = subproof3.normalize(false, false);
    println!("Normalized Proof:");
    println!("{}", normalized_proof2.render_proof_tree());
    
    let goal4 = normalized_proof2.get_goals().iter().next().unwrap()
        .to_goal().unwrap().clone();
    let context4 = goal4.get_context().clone();
    
    println!("Final Goal to Prove:");
    println!("{}", goal4.render_proof_tree());
    
    let subproof_final = generate_proof_by_intro(
        &goal4.proved_formula(),
        &"EqIntro".to_string(),
        &context4
    );
    println!("Final Subproof by Introduction:");
    println!("{}", subproof_final.render_proof_tree());

    let complete_subproof_0 = normalized_proof2.substitute(&Rc::new(MinlogProof::Goal(goal4.clone())).into(), &subproof_final.into());
    println!("Complete Subproof 0:");
    println!("{}", complete_subproof_0.render_proof_tree());
    
    let complete_subproof_1 = generate_proof_by_use(
        &goal3.proved_formula(),
        &complete_subproof_0,
        &context3
    );
    println!("Complete Subproof 1:");
    println!("{}", complete_subproof_1.render_proof_tree());
    
    let complete_subproof_2 = subproof2.substitute(&Rc::new(MinlogProof::Goal(goal3.clone())).into(), &complete_subproof_1.into());
    println!("Complete Subproof 2:");
    println!("{}", complete_subproof_2.render_proof_tree());
    
    let complete_subproof_3 = normalized_proof.substitute(&Rc::new(MinlogProof::Goal(goal2.clone())).into(), &complete_subproof_2.into());
    println!("Complete Subproof 3:");
    println!("{}", complete_subproof_3.render_proof_tree());
    
    let complete_subproof_4 = generate_proof_by_use(
        &goal1.proved_formula(),
        &complete_subproof_3,
        &context1
    );
    println!("Complete Subproof 4:");
    println!("{}", complete_subproof_4.render_proof_tree());
    
    let complete_proof_5 = subproof0.substitute(&Rc::new(MinlogProof::Goal(goal1.clone())).into(), &complete_subproof_4.into());
    println!("Complete Subproof 5:");
    println!("{}", complete_proof_5.render_proof_tree());
    
    let complete_proof = assume_proof.substitute(&Rc::new(MinlogProof::Goal(goal0.clone())).into(), &complete_proof_5.into());
    println!("Complete Proof:");
    println!("{}", complete_proof.render_proof_tree());
    
    println!("Complete Proof is valid: {}", complete_proof.is_closed());
    
    println!("Complete Proof (Unfolded):");
    println!("{}", complete_proof.unfold().render_proof_tree());
    
    let et_type = complete_proof.proved_formula().extracted_type();
    println!("Extracted Type of Complete Proof:");
    println!("{}", et_type.debug_string());
    
    let et_term = complete_proof.extracted_term();
    println!("Extracted Term of Complete Proof:");
    println!("{}", et_term.clone().map(|t| t.debug_string()).unwrap_or("None".to_string()));
    
    let zero_eq_zero = PrimeFormula::create(
        eq_pred.substitute(&tvar.clone().into(), &nat_type.clone().into()),
        vec![zero.clone(), zero.clone()]
    );
    println!("Zero Equals Zero Formula:");
    println!("{}", zero_eq_zero.debug_string());
    
    let zero_eq_intro = generate_proof_by_intro(
        &zero_eq_zero,
        &"EqIntro".to_string(),
        &ProofContext::new()
    );
    println!("Zero Equals Zero Proof by Introduction:");
    println!("{}", zero_eq_intro.render_proof_tree());
    
    let three = Application::create(
        succ.clone(),
        vec![
            Application::create(
                succ.clone(),
                vec![
                    Application::create(
                        succ.clone(),
                        vec![zero.clone()]
                    )
                ]
            )
        ]
    );
    let three_eq_three = PrimeFormula::create(
        eq_pred.substitute(&tvar.clone().into(), &nat_type.clone().into()),
        vec![three.clone(), three.clone()]
    );
    println!("Three Equals Three Formula:");
    println!("{}", three_eq_three.debug_string());
    
    let three_eq_intro = generate_proof_by_intro(
        &three_eq_three,
        &"EqIntro".to_string(),
        &ProofContext::new()
    );
    println!("Three Equals Three Proof by Introduction:");
    println!("{}", three_eq_intro.render_proof_tree());
    
    let three_comp_eq_three = Application::create(
        et_term.unwrap(),
        vec![three.clone(), three.clone(), three_eq_intro.extracted_term().unwrap()]
    );
    println!("Computed Equality Term for Three Equals Three:");
    println!("{}", three_comp_eq_three.debug_string());
    
    let normalized_three_eq_three = three_comp_eq_three.normalize(false, false);
    println!("Normalized Equality Term for Three Equals Three:");
    println!("{}", normalized_three_eq_three.debug_string());
}