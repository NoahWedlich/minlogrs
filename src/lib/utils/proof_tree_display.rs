
use std::{cmp::{max, min}, collections::btree_map::Range};

#[derive(Clone, PartialEq, Eq)]
pub enum OffsetFromCenter {
    None,
    Offset(isize),
}

impl OffsetFromCenter {
    pub fn zero() -> Self {
        OffsetFromCenter::Offset(0)
    }
    
    pub fn invert(&self) -> OffsetFromCenter {
        match self {
            OffsetFromCenter::Offset(o) => OffsetFromCenter::Offset(-o),
            OffsetFromCenter::None => OffsetFromCenter::None,
        }
    }
    
    pub fn is_none(&self) -> bool {
        matches!(self, OffsetFromCenter::None)
    }

    pub fn offset(&self) -> isize {
        match self {
            OffsetFromCenter::Offset(o) => *o,
            OffsetFromCenter::None => 0,
        }
    }

    pub fn most_extreme(v1: &OffsetFromCenter, v2: &OffsetFromCenter) -> OffsetFromCenter {
        match (v1, v2) {
            (OffsetFromCenter::Offset(o1), OffsetFromCenter::Offset(o2)) => {
                if o1.abs() >= o2.abs() {
                    OffsetFromCenter::Offset(*o1)
                } else {
                    OffsetFromCenter::Offset(*o2)
                }
            },
            (OffsetFromCenter::Offset(o), OffsetFromCenter::None) |
            (OffsetFromCenter::None, OffsetFromCenter::Offset(o)) => {
                OffsetFromCenter::Offset(*o)
            },
            (OffsetFromCenter::None, OffsetFromCenter::None) => {
                OffsetFromCenter::None
            }
        }
    }
    
    pub fn least_extreme(v1: &OffsetFromCenter, v2: &OffsetFromCenter) -> OffsetFromCenter {
        match (v1, v2) {
            (OffsetFromCenter::Offset(o1), OffsetFromCenter::Offset(o2)) => {
                if o1.abs() <= o2.abs() {
                    OffsetFromCenter::Offset(*o1)
                } else {
                    OffsetFromCenter::Offset(*o2)
                }
            },
            (OffsetFromCenter::Offset(o), OffsetFromCenter::None) |
            (OffsetFromCenter::None, OffsetFromCenter::Offset(o)) => {
                OffsetFromCenter::Offset(*o)
            },
            (OffsetFromCenter::None, OffsetFromCenter::None) => {
                OffsetFromCenter::None
            }
        }
    }
    
    pub fn recenter_at(&self, new_center: OffsetFromCenter) -> OffsetFromCenter {
        match (self, new_center) {
            (OffsetFromCenter::Offset(off), OffsetFromCenter::Offset(new_off)) => {
                OffsetFromCenter::Offset(off - new_off)
            },
            (_, _) => {
                OffsetFromCenter::None
            }
        }
    }
    
    pub fn offset_by(&self, offset: isize) -> OffsetFromCenter {
        match self {
            OffsetFromCenter::Offset(off) => {
                OffsetFromCenter::Offset(off + offset)
            },
            OffsetFromCenter::None => {
                OffsetFromCenter::None
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct OffsetRange {
    pub left: OffsetFromCenter,
    pub right: OffsetFromCenter,
}

impl OffsetRange {
    pub fn empty() -> Self {
        OffsetRange {
            left: OffsetFromCenter::zero(),
            right: OffsetFromCenter::zero(),
        }
    }
    
    pub fn from_string(s: &str) -> Self {
        let half_len = (s.chars().count() as f32) / 2.0;
        OffsetRange {
            left: OffsetFromCenter::Offset(-(half_len.floor() as isize)),
            right: OffsetFromCenter::Offset(half_len.ceil() as isize),
        }
    }
    
    pub fn union(v1: &OffsetRange, v2: &OffsetRange) -> OffsetRange {
        let lower = min(v1.left.offset(), v2.left.offset());
        let upper = max(v1.right.offset(), v2.right.offset());
        OffsetRange {
            left: OffsetFromCenter::Offset(lower),
            right: OffsetFromCenter::Offset(upper),
        }
    }

    pub fn recenter_at(&self, new_center: OffsetFromCenter) -> OffsetRange {
        OffsetRange {
            left: self.left.recenter_at(new_center.clone()),
            right: self.right.recenter_at(new_center),
        }
    }
    
    pub fn center(&self) -> OffsetFromCenter {
        match (&self.left, &self.right) {
            (OffsetFromCenter::Offset(l), OffsetFromCenter::Offset(r)) => {
                OffsetFromCenter::Offset((l + r) / 2)
            },
            _ => OffsetFromCenter::None,
        }
    }
    
    pub fn offset_by(&self, offset: isize) -> OffsetRange {
        OffsetRange {
            left: self.left.offset_by(offset),
            right: self.right.offset_by(offset),
        }
    }
    
    pub fn offset_left_by(&self, offset: isize) -> OffsetRange {
        OffsetRange {
            left: self.left.offset_by(offset),
            right: self.right.clone(),
        }
    }
    
    pub fn offset_right_by(&self, offset: isize) -> OffsetRange {
        OffsetRange {
            left: self.left.clone(),
            right: self.right.offset_by(offset),
        }
    }
    
    pub fn total_width(&self) -> Option<usize> {
        match (&self.left, &self.right) {
            (OffsetFromCenter::Offset(l), OffsetFromCenter::Offset(r)) => {
                Some((r - l) as usize)
            },
            _ => None,
        }
    }
    
    pub fn distance(&self, other: &OffsetRange) -> isize {
        match (&self.right, &other.left) {
            (OffsetFromCenter::Offset(r1), OffsetFromCenter::Offset(l2)) => {
                l2 - r1
            },
            _ => 0,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RangedString {
    pub content: String,
    pub range: OffsetRange,
}

impl RangedString {
    pub fn new(content: String, range: OffsetRange) -> Self {
        RangedString { content, range }
    }
    
    pub fn join(left: &RangedString, right: &RangedString) -> Self {
        if left.content.is_empty() {
            return right.clone();
        }

        if right.content.is_empty() {
            return left.clone();
        }

        let mut content = left.content.clone();

        let distance = left.range.distance(&right.range);
        if distance >= 0 {
            content += &" ".repeat(distance as usize);
            content += &right.content;

            let range = OffsetRange {
                left: left.range.left.clone(),
                right: right.range.right.clone(),
            };

            RangedString { content, range }
        } else {
            let overlap = (-distance) as usize;

            let left_chars: Vec<char> = left.content.chars().collect();
            let right_chars: Vec<char> = right.content.chars().collect();

            if overlap >= right_chars.len() {
                RangedString { content, range: left.range.clone() }
            } else {
                let suffix: String = right_chars.iter().skip(overlap).collect();
                content = left_chars.into_iter().collect::<String>() + &suffix;

                let range = OffsetRange::union(&left.range, &right.range);

                RangedString { content, range }
            }
        }
    }
    
    pub fn join_multiple(strings: &[RangedString]) -> Self {
        let mut result = strings[0].clone();
        
        for s in strings.iter().skip(1) {
            result = RangedString::join(&result, s);
        }
        
        result
    }
    
    pub fn embed_into(&self, range: &OffsetRange) -> Self {
        let left_padding = (range.left.offset() - self.range.left.offset()).unsigned_abs();
        let right_padding = (range.right.offset() - self.range.right.offset()).unsigned_abs();
        
        RangedString {
            content: format!(
                "{}{}{}",
                " ".repeat(left_padding),
                self.content,
                " ".repeat(right_padding)
            ),
            range: range.clone(),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ProofTreeNode {
    Leaf(String),
    Node {
        premises: Vec<(ProofTreeNode, OffsetFromCenter)>,
        conclusion: String,
        label: Option<String>,
    },
}

impl ProofTreeNode {
    pub fn new_leaf(conclusion: String) -> Self {
        ProofTreeNode::Leaf(conclusion)
    }
    
    pub fn new_node(premises: Vec<ProofTreeNode>, conclusion: String, label: Option<String>) -> Self {
        ProofTreeNode::Node {
            premises: premises.into_iter().map(|p| (p, OffsetFromCenter::zero())).collect(),
            conclusion,
            label,
        }
    }
    
    pub fn get_reserved_range(&self, layer: usize) -> Option<OffsetRange> {
        match self {
            ProofTreeNode::Leaf(text) => {
                if layer == 0 {
                    Some(OffsetRange::from_string(text))
                } else {
                    None
                }
            },
            ProofTreeNode::Node { premises, conclusion, label } => {
                match layer {
                    0 => {
                        Some(OffsetRange::from_string(conclusion))
                    },
                    1 => {
                        let most_extreme_span = OffsetRange::union(
                            &self.get_reserved_range(0).unwrap_or(OffsetRange::empty()),
                            &self.get_reserved_range(2).unwrap_or(OffsetRange::empty())
                        );
                        
                        let label_length = label.as_ref().map_or(0, |l| l.chars().count() + 1) as isize;
                        Some(most_extreme_span.offset_right_by(label_length))
                    },
                    _ => {
                        if premises.len() == 1 {
                            premises[0].0.get_reserved_range(layer - 2)
                        } else if premises.len() > 1 {
                            let (left_elem, left_elem_offset) = premises.first().unwrap();
                            let (right_elem, right_elem_offset) = premises.last().unwrap();
                            
                            let left_offset = left_elem.get_reserved_range(layer - 2)
                                .map_or(OffsetFromCenter::zero(), |r| r.recenter_at(left_elem_offset.invert()).left);
                            
                            let right_offset = right_elem.get_reserved_range(layer - 2)
                                .map_or(OffsetFromCenter::zero(), |r| r.recenter_at(right_elem_offset.invert()).right);
                            
                            Some(OffsetRange {
                                left: left_offset,
                                right: right_offset,
                            })
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }
    
    pub fn get_max_reserved_range(&self) -> OffsetRange {
        let mut most_extreme = OffsetRange::empty();
          
        for d in 0..self.max_depth() {
            let layer_range = self.get_reserved_range(d)
                .unwrap_or(OffsetRange::empty());
            most_extreme = OffsetRange::union(&most_extreme, &layer_range);
        }

        most_extreme
    }
    
    pub fn max_depth(&self) -> usize {
        match self {
            ProofTreeNode::Leaf(_) => 1,
            ProofTreeNode::Node { premises, .. } => {
                2 + premises.iter().map(|(elem, _)| elem.max_depth()).max().unwrap_or(0)
            }
        }
    }
    
    pub fn layout(&mut self) {
        match self {
            ProofTreeNode::Leaf(_) => {},
            ProofTreeNode::Node { premises, .. } => {
                for (premise, _) in premises.iter_mut() {
                    premise.layout();
                }
                
                let premise_count = premises.len();

                for i in 0..(premise_count - 1) {
                    let (old_premises, future_premises) = premises.split_at_mut(i + 1);
                    
                    let (current_premise, current_offset) = &mut old_premises[i];
                    for j in (i + 1)..premise_count {
                        let (next_premise, next_offset) = &mut future_premises[j - (i + 1)];
                        
                        *next_offset = OffsetFromCenter::most_extreme(next_offset, current_offset);
                        
                        for depth in 0..max(current_premise.max_depth(), next_premise.max_depth()) {
                            let current_range = current_premise.get_reserved_range(depth)
                                .map(|r| r.recenter_at(current_offset.invert()));
                            
                            let next_range = next_premise.get_reserved_range(depth)
                                .map(|r| r.recenter_at(next_offset.invert()));

                            if let (Some(cr), Some(nr)) = (current_range, next_range) {
                                let distance = cr.distance(&nr);
                                if distance < 3 {
                                    let shift_amount = 3 - distance;
                                    *next_offset = next_offset.offset_by(shift_amount);
                                }
                            }
                            
                        }
                    }
                }
                
                let mut entire_range = OffsetRange::empty();
                for (premise, offset) in premises.iter() {
                    let rng = premise.get_reserved_range(0).unwrap()
                        .recenter_at(offset.invert());
                    entire_range = OffsetRange::union(&entire_range, &rng);
                }
                
                let center = entire_range.center();
                for (_, offset) in premises.iter_mut() {
                    *offset = offset.recenter_at(center.clone());
                }
            }
        }
    }
    
    pub fn get_layer(&self, depth: usize) -> RangedString {
        match self {
            ProofTreeNode::Leaf(text) => {
                if depth == 0 {
                    RangedString::new(text.clone(), OffsetRange::from_string(text))
                } else {
                    RangedString::new(String::new(), OffsetRange::empty())
                }
            },
            ProofTreeNode::Node { premises, conclusion, label } => {
                match depth {
                    0 => {
                        RangedString::new(conclusion.clone(), OffsetRange::from_string(conclusion))
                    },
                    1 => {
                        let line_range_with_label = self.get_reserved_range(1).unwrap();

                        let label_length = label.as_ref().map_or(0, |l| l.chars().count() + 1);
                        let line_range = line_range_with_label.offset_right_by(-(label_length as isize));
                        
                        let conclusion_range = self.get_reserved_range(0).unwrap();
                        let conclusion_center = conclusion_range.center();
                        
                        let premise_centers: Vec<isize> = premises.iter()
                            .map(|(p, o)| {
                                let r = p.get_reserved_range(0).unwrap();
                                r.center().recenter_at(o.invert()).offset()
                            })
                            .collect();
                        
                        let mut line_string = String::new();
                        for offset in line_range.left.offset()..line_range.right.offset() {
                            if offset == conclusion_center.offset() {
                                if premise_centers.contains(&offset) {
                                    line_string += "╋";
                                } else {
                                    line_string += "┳";
                                }
                            } else if !premise_centers.is_empty() {
                                if offset == *premise_centers.first().unwrap() {
                                    if offset == line_range.left.offset() {
                                        line_string += "┗";
                                    } else {
                                        line_string += "┻";
                                    }
                                } else if offset == *premise_centers.last().unwrap() {
                                    if offset == line_range.right.offset() - 1 {
                                        line_string += "┛";
                                    } else {
                                        line_string += "┻";
                                    }
                                } else if premise_centers.contains(&offset) {
                                    line_string += "┻";
                                } else {
                                    line_string += "━";
                                }
                            } else {
                                line_string += "━";
                            }
                        }
                        
                        let line_string = RangedString::new(
                            line_string,
                            line_range.clone(),
                        );
                        
                        let label_string = RangedString::new(
                            label.as_ref().map_or(String::new(), |l| format!(" {}", l)),
                            OffsetRange {
                                left: line_range.right.clone(),
                                right: line_range_with_label.right.clone(),
                            },
                        );
                        
                        RangedString::join(&line_string, &label_string)
                    }
                    _ => {
                        if premises.is_empty() {
                            RangedString::new(String::new(), OffsetRange::empty())
                        } else {
                            let mut premise_layers = vec![];
                            for (premise, offset) in premises.iter() {
                                let layer = premise.get_layer(depth - 2);
                                let recentered_range = layer.range.recenter_at(offset.invert());
                                premise_layers.push(RangedString {
                                    content: layer.content,
                                    range: recentered_range,
                                });
                            }
                            
                            RangedString::join_multiple(&premise_layers)
                        }
                    }
                }
            }
        }
    }
    
    pub fn render(&self) -> String {
        let depth = self.max_depth();
        let mut result = String::new();
        
        let entire_range = self.get_max_reserved_range();

        for d in (0..depth).rev() {
            let ranged_layer = self.get_layer(d).embed_into(&entire_range);
            result += &ranged_layer.content;
            if d > 0 {
                result += "\n";
            }
        }
        
        result
    }
}

// Example usage:
pub fn example_proof_tree() {
    // u0: A    u1: A -> B 
    // ------------------- -> (-)
    //         B    u2: B -> C
    //         --------------- -> (-)
    //                 C
    //             ------ -> (+, u0)
    //             A -> C
    //     ------------------ -> (+, u1)
    //     (B -> C) -> A -> C
    // ------------------------------ -> (+, u2)
    // (A -> B) -> (B -> C) -> A -> C
    
    let ass_u0 = ProofTreeNode::new_leaf("u0: A".to_string());
    let ass_u1 = ProofTreeNode::new_leaf("u1: A -> B".to_string());
    let ass_u2 = ProofTreeNode::new_leaf("u2: B -> C".to_string());

    let node0 = ProofTreeNode::new_node(vec![ass_u0.clone(), ass_u1.clone()], "B".to_string(), Some("-> (-)".to_string()));
    let node1 = ProofTreeNode::new_node(vec![node0.clone(), ass_u2.clone()], "C".to_string(), Some("-> (-)".to_string()));
    let node2 = ProofTreeNode::new_node(vec![node1.clone()], "A -> C".to_string(), Some("-> (+, u0)".to_string()));
    let node3 = ProofTreeNode::new_node(vec![node2.clone()], "(B -> C) -> A -> C".to_string(), Some("-> (+, u1)".to_string()));
    let mut node4 = ProofTreeNode::new_node(vec![node3.clone()], "(A -> B) -> (B -> C) -> A -> C".to_string(), Some("-> (+, u2)".to_string()));

    node4.layout();

    println!("{}", node4.render());
}

pub fn example_proof_tree_2() {
    let a = ProofTreeNode::new_leaf("A".to_string());
    let b = ProofTreeNode::new_leaf("B".to_string());
    let c = ProofTreeNode::new_leaf("C".to_string());
    let d = ProofTreeNode::new_leaf("D".to_string());
    let e = ProofTreeNode::new_leaf("E".to_string());
    let f = ProofTreeNode::new_leaf("F".to_string());
    let g = ProofTreeNode::new_leaf("G".to_string());
    let h = ProofTreeNode::new_leaf("H".to_string());
    let i = ProofTreeNode::new_leaf("I".to_string());
    let j = ProofTreeNode::new_leaf("J".to_string());

    // Simple pairwise combinations
    let n_ab = ProofTreeNode::new_node(vec![a.clone(), b.clone()], "A ∧ B".to_string(), Some("∧-intro".to_string()));
    let n_cd = ProofTreeNode::new_node(vec![c.clone(), d.clone()], "C ∧ D".to_string(), Some("∧-intro".to_string()));
    let n_ef = ProofTreeNode::new_node(vec![e.clone(), f.clone()], "E ∧ F".to_string(), Some("∧-intro".to_string()));
    let n_ghi = ProofTreeNode::new_node(vec![g.clone(), h.clone(), i.clone()], "G ∧ H ∧ I".to_string(), Some("∧-intro (3)".to_string()));

    // Use elimination/intro steps with varied label lengths
    let n_ab_to_x = ProofTreeNode::new_node(vec![n_ab.clone()], "X".to_string(), Some("→-elim (long label)".to_string()));
    let n_cd_to_y = ProofTreeNode::new_node(vec![n_cd.clone()], "Y".to_string(), Some("→-elim".to_string()));
    let n_ef_to_z = ProofTreeNode::new_node(vec![n_ef.clone()], "Z".to_string(), Some("⊃-intro".to_string()));

    // Combine multiple subproofs into a wider node
    let n_wide = ProofTreeNode::new_node(
        vec![n_ab_to_x.clone(), n_cd_to_y.clone(), n_ef_to_z.clone()],
        "M".to_string(),
        Some("multi-join".to_string()),
    );

    // Nest further to increase complexity
    let n_deeper = ProofTreeNode::new_node(
        vec![n_wide.clone(), n_ghi.clone(), j.clone()],
        "Final-Intermediate".to_string(),
        Some("complex-rule (very long label to test spacing)".to_string()),
    );

    // Single-premise transformations and a final root
    let n_single = ProofTreeNode::new_node(vec![n_deeper.clone()], "Transformer".to_string(), Some("single-step".to_string()));
    // Include the three-branch subtree into the existing tree (no testing logic).
    let small_left = ProofTreeNode::new_leaf("s".to_string());
    let small_mid = ProofTreeNode::new_leaf("s".to_string());
    let small_right = ProofTreeNode::new_leaf("s".to_string());

    let long_leaf = |id: usize| {
        ProofTreeNode::new_leaf(format!("LONG_{}_{}", id, "X".repeat(30)))
    };

    let wide_left = ProofTreeNode::new_node(
        vec![
            long_leaf(1),
            long_leaf(2),
            long_leaf(3),
            long_leaf(4),
        ],
        "WLEFT".to_string(),
        Some("wide".to_string()),
    );

    let wide_right = ProofTreeNode::new_node(
        vec![
            long_leaf(5),
            long_leaf(6),
            long_leaf(7),
            long_leaf(8),
        ],
        "WRIGHT".to_string(),
        Some("wide".to_string()),
    );

    let left_sub = ProofTreeNode::new_node(vec![small_left, wide_left], "L".to_string(), Some("l".to_string()));
    let mid_sub = ProofTreeNode::new_node(vec![small_mid], "M".to_string(), Some("m".to_string()));
    let right_sub = ProofTreeNode::new_node(vec![small_right, wide_right], "R".to_string(), Some("r".to_string()));

    let three_branch = ProofTreeNode::new_node(vec![left_sub, mid_sub, right_sub], "ROOT".to_string(), Some("root".to_string()));

    let mut root = ProofTreeNode::new_node(vec![n_single.clone(), three_branch], "Ultimate-Goal".to_string(), Some("∴".to_string()));
    root.layout();
    let output = root.render();
    std::fs::write("proof_tree.txt", output).expect("Failed to write proof tree to file");
}