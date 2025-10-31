
use std::rc::Rc;
pub trait PrettyPrintable {
    fn to_pp_element(&self, detail: bool) -> PPElement;
    
    fn to_enclosed_pp_element(&self, detail: bool) -> PPElement {
        if self.requires_parens(detail) {
            PPElement::group(vec![
                PPElement::text(self.open_paren()),
                self.to_pp_element(detail),
                PPElement::text(self.close_paren())
            ], BreakType::Consistent, 0)
        } else {
            self.to_pp_element(detail)
        }
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        true
    }
    
    fn open_paren(&self) -> String {
        "(".to_string()
    }
    
    fn close_paren(&self) -> String {
        ")".to_string()
    }
    
    fn render(&self, detail: bool, max_width: usize) -> String {
        let mut element = self.to_pp_element(detail);
        element.layout(max_width, max_width, 0);
        element.to_string()
    }
    
    fn debug_string(&self) -> String {
        self.render(true, 100)
    }
    
    fn display_string(&self) -> String {
        self.render(false, 100)
    }
}

impl<T: PrettyPrintable> PrettyPrintable for Rc<T> {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        self.as_ref().to_pp_element(detail)
    }
    
    fn requires_parens(&self, detail: bool) -> bool {
        self.as_ref().requires_parens(detail)
    }
    
    fn open_paren(&self) -> String {
        self.as_ref().open_paren()
    }
    
    fn close_paren(&self) -> String {
        self.as_ref().close_paren()
    }
}

crate::wrapper_enum! {
    
    pub trait PPElementBody: Clone {
        pub fn max_size(&Self) -> usize
        pub fn layout(&mut Self, max_width: usize, width_left: usize, indent: usize) -> (usize, usize)
        
        pub fn to_string(&Self) -> String
    }

    #[derive(Debug, Clone)]
    pub enum PPElement {
        Text(|text| TextElement),
        Break(|break| BreakElement),
        Group(|group| GroupElement),
    }
}

impl PPElement {
    pub fn text(text: String) -> PPElement {
        PPElement::Text(TextElement { text })
    }
    
    pub fn break_elem(space: usize, indent: usize, breaks: bool) -> PPElement {
        PPElement::Break(BreakElement { space, indent, breaks })
    }
    
    pub fn group(elements: Vec<PPElement>, break_type: BreakType, indent: usize) -> PPElement {
        PPElement::Group(GroupElement { elements, break_type, indent })
    }
    
    pub fn list(elements: Vec<PPElement>, first_break: PPElement, separator: PPElement, sec_break: PPElement, break_type: BreakType) -> PPElement {
        let mut grouped_elements = vec![];
        
        let element_count = elements.len();
        for (i, element) in elements.into_iter().enumerate() {
            grouped_elements.push(
                if i < element_count - 1 {
                    PPElement::group(vec![
                        element,
                        first_break.clone(),
                        separator.clone(),
                    ], BreakType::Flexible, 0)
                } else {
                    element
                }
            );
            
            if i < element_count - 1 {
                grouped_elements.push(sec_break.clone());
            }
        }
        
        PPElement::group(grouped_elements, break_type, 0)
    }
}

#[derive(Debug, Clone)]
pub struct TextElement {
    text: String,
}

impl TextElement {
    pub fn create(text: String) -> PPElement {
        PPElement::Text(TextElement { text })
    }
}

impl PPElementBody for TextElement {
    fn max_size(&self) -> usize {
        self.text.len()
    }
    
    fn layout(&mut self, max_width: usize, width_left: usize, indent: usize) -> (usize, usize) {
        if self.text.len() <= width_left {
            (max_width, width_left - self.text.len())
        } else {
            let splits_needed = (self.text.len() - width_left) / (max_width - indent);
            let remainder = (self.text.len() - width_left) % max_width;
            
            let mut result = String::new();
            let chars = self.text.chars().collect::<Vec<_>>();

            for char in chars.iter().take(width_left) {
                result.push(*char);
            }
            
            result.push('\n');
            result.push_str(&" ".repeat(indent));
            
            for split in 0..splits_needed {
                for i in 0..max_width {
                    result.push(chars[width_left + split * max_width + i]);
                }
                result.push('\n');
                result.push_str(&" ".repeat(indent));
            }
            
            for i in 0..remainder {
                result.push(chars[width_left + splits_needed * max_width + i]);
            }
            
            self.text = result;
            
            (max_width, max_width - remainder)
        }
    }
    
    fn to_string(&self) -> String {
        self.text.clone()
    }
}

#[derive(Debug, Clone)]
pub struct BreakElement {
    space: usize,
    indent: usize,
    breaks: bool,
}

impl BreakElement {
    pub fn create(space: usize, indent: usize, breaks: bool) -> PPElement {
        PPElement::Break(BreakElement { space, indent, breaks })
    }
}

impl PPElementBody for BreakElement {
    fn max_size(&self) -> usize {
        self.space
    }
    
    fn layout(&mut self, max_width: usize, width_left: usize, indent: usize) -> (usize, usize) {
        if self.breaks {
            self.indent += indent;
            (max_width, max_width - self.indent)
        } else if self.space > width_left {
            self.indent += indent;
            self.breaks = true;
            (max_width, max_width - self.indent)
        } else {
            (max_width, width_left - self.space)
        }
    }
    
    fn to_string(&self) -> String {
        if self.breaks {
            "\n".to_string() + &" ".repeat(self.indent)
        } else {
            " ".repeat(self.space)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakType {
    Consistent,
    Flexible,
}

#[derive(Debug, Clone)]
pub struct GroupElement {
    elements: Vec<PPElement>,
    break_type: BreakType,
    indent: usize,
}

impl GroupElement {
    pub fn create(elements: Vec<PPElement>, break_type: BreakType, indent: usize) -> PPElement {
        PPElement::Group(GroupElement { elements, break_type, indent })
    }
}

impl PPElementBody for GroupElement {
    fn max_size(&self) -> usize {
        self.elements.iter().map(|e| e.max_size()).sum()
    }
    
    fn layout(&mut self, max_width: usize, width_left: usize, indent: usize) -> (usize, usize) {
        let any_breaks = self.max_size() > width_left;
        if !any_breaks {
            let mut current_width_left = width_left;
            
            for element in &mut self.elements {
                let (_, new_width_left) = element.layout(max_width, current_width_left, indent + self.indent);
                current_width_left = new_width_left;
            }
            
            (max_width, current_width_left)
        } else {
            let mut sections = vec![];
            
            let mut current_section = vec![];
            let mut current_break = None;
            
            for element in &self.elements {
                match element {
                    PPElement::Break(break_elem) => {
                        if !current_section.is_empty() {
                            sections.push((current_section, current_break));
                            current_section = vec![];
                        }
                        current_break = Some(break_elem);
                    }
                    _ => {
                        current_section.push(element);
                    }
                }
            }
            
            if !current_section.is_empty() || current_break.is_some() {
                sections.push((current_section, current_break));
            }
            
            let mut new_elements = vec![];
            
            let mut current_width_left = width_left;
            
            for (section, break_elem_opt) in sections {
                let mut new_indent = 0;
                
                if let Some(break_elem) = break_elem_opt {
                    let mut break_elem = break_elem.clone();
                    
                    if let BreakType::Consistent = self.break_type {
                        new_indent = break_elem.indent;
                        current_width_left = width_left - break_elem.indent;
                        break_elem.breaks = true;
                    } else {
                        let size = section.iter().map(|e| e.max_size()).sum::<usize>() + break_elem.space;
                        if size > current_width_left && (size <= max_width - indent || 3*size > 4*current_width_left) {
                            new_indent = break_elem.indent;
                            current_width_left = max_width - break_elem.indent;
                            break_elem.breaks = true;
                        } else {
                            current_width_left -= break_elem.space;
                            break_elem.breaks = false;
                        }
                    }
                    
                    break_elem.layout(max_width, current_width_left, indent + self.indent);
                    new_elements.push(PPElement::Break(break_elem));
                }
                
                for elem in section {
                    let mut elem = elem.clone();
                    
                    if max_width <= new_indent {
                        new_indent = indent;
                    }
                    
                    let (_, new_width_left_elem) = elem.layout(max_width - new_indent, current_width_left, indent + self.indent + new_indent);
                    current_width_left = new_width_left_elem;
                    new_elements.push(elem);
                }
            }
            
            self.elements = new_elements;
            (max_width, current_width_left)
        }
    }
    
    fn to_string(&self) -> String {
        let mut result = String::new();
        
        for element in &self.elements {
            let inner_result = element.to_string();
            let indented = inner_result.replace("\n", &("\n".to_string() + &" ".repeat(self.indent)));
            result.push_str(&indented);
        }
        
        result
    }
}