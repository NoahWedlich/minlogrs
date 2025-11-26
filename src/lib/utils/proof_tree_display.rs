
use crate::includes::essential::*;

pub trait ProofTreeDisplayable {
    fn to_proof_tree_node(&self) -> ProofTreeNode;
    
    fn render_proof_tree(&self) -> String {
        let mut tree = self.to_proof_tree_node();
        tree.layout();
        tree.render()
    }
}

impl<T: ProofTreeDisplayable> ProofTreeDisplayable for Rc<T> {
    fn to_proof_tree_node(&self) -> ProofTreeNode {
        self.as_ref().to_proof_tree_node()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TextSpan {
    pub left: isize,
    pub right: isize,
}

impl TextSpan {
    pub fn empty() -> Self {
        TextSpan {
            left: 0,
            right: 0,
        }
    }
    
    pub fn from_string(s: &str) -> Self {
        let half_len = (s.chars().count() as f32) / 2.0;
        TextSpan {
            left: -(half_len.floor() as isize),
            right: half_len.ceil() as isize,
        }
    }
    
    pub fn span(v1: &TextSpan, v2: &TextSpan) -> TextSpan {
        TextSpan {
            left: min(v1.left, v2.left),
            right: max(v1.right, v2.right),
        }
    }
    
    pub fn span_multiple(spans: &[TextSpan]) -> TextSpan {
        let lower = spans.iter().map(|s| s.left).min().unwrap_or(0);
        let upper = spans.iter().map(|s| s.right).max().unwrap_or(0);
        TextSpan {
            left: lower,
            right: upper,
        }
    }
    
    pub fn center(&self) -> isize {
        self.left + ((self.right - self.left) / 2)
    }
    
    pub fn offset_by(&self, offset: isize) -> TextSpan {
        TextSpan {
            left: self.left + offset,
            right: self.right + offset,
        }
    }
    
    pub fn offset_left_by(&self, offset: isize) -> TextSpan {
        TextSpan {
            left: self.left + offset,
            right: self.right,
        }
    }
    
    pub fn offset_right_by(&self, offset: isize) -> TextSpan {
        TextSpan {
            left: self.left,
            right: self.right + offset,
        }
    }
    
    pub fn align_left_to(&self, new_left: isize) -> TextSpan {
        let shift_amount = new_left - self.left;
        self.offset_by(shift_amount)
    }
    
    pub fn align_right_to(&self, new_right: isize) -> TextSpan {
        let shift_amount = new_right - self.right;
        self.offset_by(shift_amount)
    }
    
    pub fn align_center_to(&self, new_center: isize) -> TextSpan {
        let center = self.center();
        let shift_amount = new_center - center;
        self.offset_by(shift_amount)
    }
    
    pub fn total_width(&self) -> usize {
        (self.right - self.left).unsigned_abs()
    }
    
    pub fn distance(&self, other: &TextSpan) -> isize {
        other.left - self.right
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RangedString {
    pub content: String,
    pub range: TextSpan,
}

impl RangedString {
    pub fn empty() -> Self {
        RangedString {
            content: String::new(),
            range: TextSpan::empty(),
        }
    }

    pub fn new(content: String, range: TextSpan) -> Self {
        if content.chars().count() != range.total_width() {
            panic!("Content length does not match range width");
        }
        
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

            let range = TextSpan {
                left: left.range.left,
                right: right.range.right,
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

                let range = TextSpan::span(&left.range, &right.range);

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
    
    pub fn embed_into(&self, range: &TextSpan) -> Self {
        let left_padding = (range.left - self.range.left).unsigned_abs();
        let right_padding = (range.right - self.range.right).unsigned_abs();
        
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
    Leaf(Vec<RangedString>),
    Node {
        premises: Vec<ProofTreeNode>,
        conclusion: Vec<RangedString>,
        label: Option<String>,
    },
}

impl ProofTreeNode {
    pub fn new_leaf(conclusion: String) -> Self {
        ProofTreeNode::Leaf(conclusion.lines().rev()
            .map(|s| RangedString::new(s.to_string(), TextSpan::from_string(s))).collect())
    }
    
    pub fn new_node(premises: Vec<ProofTreeNode>, conclusion: String, label: Option<String>) -> Self {
        ProofTreeNode::Node {
            premises,
            conclusion: conclusion.lines().rev()
                .map(|s| RangedString::new(s.to_string(), TextSpan::from_string(s))).collect(),
            label,
        }
    }
    
    pub fn offset_by(&mut self, offset: isize) {
        match self {
            ProofTreeNode::Leaf(text) => {
                for line in text.iter_mut() {
                    line.range = line.range.offset_by(offset);
                }
            },
            ProofTreeNode::Node { premises, conclusion, .. } => {
                for line in conclusion.iter_mut() {
                    line.range = line.range.offset_by(offset);
                }
                
                for premise in premises.iter_mut() {
                    premise.offset_by(offset);
                }
            }
        }
    }
    
    pub fn max_depth(&self) -> usize {
        match self {
            ProofTreeNode::Leaf(text) => {
                text.len()
            },
            ProofTreeNode::Node { premises, conclusion, .. } => {
                conclusion.len() + 1 + premises.iter().map(|elem| elem.max_depth()).max().unwrap_or(1)
            }
        }
    }
    
    pub fn get_reserved_range(&self, layer: usize) -> Option<TextSpan> {
        match self {
            ProofTreeNode::Leaf(text) => {
                if layer < text.len() {
                    Some(TextSpan::span_multiple(
                        text.iter().map(|line| line.range.clone()).collect::<Vec<TextSpan>>().as_slice()
                    ))
                } else if layer == 0 && text.is_empty() {
                    Some(TextSpan{ left: 0, right: 1 })
                } else {
                    None
                }
            },
            ProofTreeNode::Node { premises, conclusion, label } => {
                if layer < conclusion.len() {
                    Some(conclusion[layer].range.clone())
                } else if layer == conclusion.len() {
                    let line_range = TextSpan::span_multiple(
                        &(0..conclusion.len())
                            .filter_map(|d| self.get_reserved_range(d))
                            .chain(std::iter::once(
                                self.get_reserved_range(conclusion.len() + 1).unwrap_or(TextSpan::empty())
                            )).collect::<Vec<TextSpan>>()
                    );
                    
                    let label_length = label.as_ref().map_or(0, |l| l.chars().count() + 1) as isize;
                    Some(line_range.offset_right_by(label_length))
                } else if premises.len() > 1 {
                    let left_elem = premises.first().unwrap();
                    let right_elem = premises.last().unwrap();
                    
                    let left_offset = left_elem.get_reserved_range(layer - conclusion.len() - 1)
                        .map_or(0, |r| r.left);

                    let right_offset = right_elem.get_reserved_range(layer - conclusion.len() - 1)
                        .map_or(0, |r| r.right);
                    
                    Some(TextSpan {
                        left: left_offset,
                        right: right_offset,
                    })
                } else if premises.len() == 1 {
                    premises[0].get_reserved_range(layer - conclusion.len() - 1)
                } else if layer == conclusion.len() + 1 {
                    let max_conclusion_range = TextSpan::span_multiple(
                        &(0..conclusion.len())
                            .filter_map(|d| self.get_reserved_range(d))
                            .collect::<Vec<TextSpan>>()
                    );
                    
                    let center = max_conclusion_range.center();
                    
                    Some(TextSpan {
                        left: center,
                        right: center + 1,
                    })
                } else {
                    None
                }
            }
        }
    }
    
    pub fn get_max_reserved_range(&self) -> TextSpan {
        TextSpan::span_multiple(
            &(0..self.max_depth())
                .filter_map(|d| self.get_reserved_range(d))
                .collect::<Vec<TextSpan>>()
        )
    }
    
    pub fn layout(&mut self) {
        let mut left_boundaries = vec![0; self.max_depth()];
        self.layout_recursive(&mut left_boundaries, 0);
    }
    
    fn layout_recursive(&mut self, left_boundaries: &mut Vec<isize>, bottom_layer: usize) {
        match self {
            ProofTreeNode::Leaf(text) => {
                let mut furthest_left = text.iter()
                    .map(|line| line.range.left)
                    .min().unwrap_or(isize::MAX);
                
                for i in 0..text.len() {
                    let left_boundary = left_boundaries[bottom_layer + i];
                    let shift_amount = (left_boundary + if left_boundary == 0 { 0 } else { 3 }).saturating_sub(furthest_left);
                    
                    if shift_amount > 0 {
                        for line in text.iter_mut() {
                            line.range = line.range.offset_by(shift_amount);
                        }
                        
                        furthest_left += shift_amount;
                    }
                }
                
                let furthest_right = text.iter()
                    .map(|line| line.range.right)
                    .max().unwrap_or(isize::MIN);
                
                for i in 0..text.len() {
                    left_boundaries[bottom_layer + i] = max(left_boundaries[bottom_layer + i], furthest_right);
                }
                
                for line in text.iter_mut() {
                    line.range = line.range.align_left_to(furthest_left);
                }
            },
            ProofTreeNode::Node { premises, conclusion, label } => {
                for premise in premises.iter_mut() {
                    premise.layout_recursive(left_boundaries, bottom_layer + 1 + conclusion.len());
                }
                
                let mut premise_ground_range = TextSpan::span_multiple(
                    &premises.iter()
                        .filter_map(|p| p.get_reserved_range(0))
                        .collect::<Vec<TextSpan>>()
                );
                let center = premise_ground_range.center();
                
                for line in conclusion.iter_mut() {
                    line.range = line.range.align_center_to(center);
                }
                
                let mut furthest_left = conclusion.iter()
                    .map(|line| line.range.left)
                    .min().unwrap_or(isize::MAX);
                
                for i in 0..conclusion.len() {
                    let left_boundary = left_boundaries[bottom_layer + i];
                    let shift_amount = (left_boundary + if left_boundary == 0 { 0 } else { 3 }).saturating_sub(furthest_left);
                    
                    if shift_amount > 0 {
                        for line in conclusion.iter_mut() {
                            line.range = line.range.offset_by(shift_amount);
                        }
                        
                        for premise in premises.iter_mut() {
                            premise.offset_by(shift_amount);
                        }
                        
                        for i in 0..premises.iter().map(|p| p.max_depth()).max().unwrap_or(0) {
                            left_boundaries[bottom_layer + conclusion.len() + 1 + i] += shift_amount;
                        }
                        
                        furthest_left += shift_amount;
                        premise_ground_range = premise_ground_range.offset_by(shift_amount);
                    }
                }
                
                let conclusion_range = TextSpan::span_multiple(
                    &conclusion.iter()
                        .map(|line| line.range.clone())
                        .collect::<Vec<TextSpan>>()
                );
                
                for line in conclusion.iter_mut() {
                    line.range = line.range.align_left_to(furthest_left);
                }
                
                for i in 0..conclusion.len() {
                    left_boundaries[bottom_layer + i] = max(left_boundaries[bottom_layer + i], conclusion_range.right);
                }
                
                let line_range = TextSpan::span(&premise_ground_range, &conclusion_range);
                let left_boundary = left_boundaries[bottom_layer + conclusion.len()];
                let shift_amount = (left_boundary + if left_boundary == 0 { 0 } else { 3 }).saturating_sub(line_range.left);
                if shift_amount > 0 {
                    for line in conclusion.iter_mut() {
                        line.range = line.range.offset_by(shift_amount);
                    }
                    
                    for premise in premises.iter_mut() {
                        premise.offset_by(shift_amount);
                    }
                    
                    left_boundaries[bottom_layer..].iter_mut().for_each(|b| *b += shift_amount);
                }
                
                let label_length = label.as_ref().map_or(0, |l| l.chars().count() + 1) as isize;
                left_boundaries[bottom_layer + conclusion.len()] = max(
                    left_boundaries[bottom_layer + conclusion.len()],
                    line_range.right + (if shift_amount > 0 { shift_amount } else { 0 }) + label_length
                );
            }
        }
    }
    
    pub fn get_layer(&self, depth: usize) -> RangedString {
        match self {
            ProofTreeNode::Leaf(text) => {
                text.get(depth).cloned().unwrap_or(RangedString::empty())
            },
            ProofTreeNode::Node { premises, conclusion, label } => {
                if depth < conclusion.len() {
                    conclusion[depth].clone()
                } else if depth == conclusion.len() {
                    let line_range_with_label = self.get_reserved_range(depth).unwrap();
                    
                    let label_length = label.as_ref().map_or(0, |l| l.chars().count() + 1);
                    let line_range = line_range_with_label.offset_right_by(-(label_length as isize));
                    
                    let conclusion_range = TextSpan::span_multiple(
                        &(0..conclusion.len())
                            .filter_map(|d| self.get_reserved_range(d))
                            .collect::<Vec<TextSpan>>()
                    );
                    let conclusion_center = conclusion_range.center();
                    
                    let premise_centers: Vec<isize> = premises.iter()
                        .map(|p| {
                            let r = p.get_reserved_range(0).unwrap();
                            r.center()
                        })
                        .collect();
                    
                    let mut line_string = String::new();
                    for offset in line_range.left..line_range.right {
                        if offset == conclusion_center {
                            if premise_centers.contains(&offset) {
                                line_string += "╋";
                            } else {
                                line_string += "┳";
                            }
                        } else if !premise_centers.is_empty() {
                            if offset == *premise_centers.first().unwrap() {
                                if offset == line_range.left {
                                    line_string += "┗";
                                } else {
                                    line_string += "┻";
                                }
                            } else if offset == *premise_centers.last().unwrap() {
                                if offset == line_range.right - 1 {
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
                    
                    if line_string.is_empty() {
                        RangedString::empty()
                    } else if let Some(lbl) = label {
                        line_string += &format!(" {}", lbl);
                        RangedString::new(
                            line_string,
                            line_range_with_label,
                        )
                    } else {
                        RangedString::new(
                            line_string,
                            line_range,
                        )
                    }
                } else if !premises.is_empty() {
                    let premise_strings = premises.iter()
                        .map(|p| p.get_layer(depth - conclusion.len() - 1))
                        .collect::<Vec<RangedString>>();
                    RangedString::join_multiple(&premise_strings)
                } else {
                    RangedString::empty()
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