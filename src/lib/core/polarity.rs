
use crate::utils::pretty_printer::{PrettyPrintable, PPElement, BreakType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Polarity {
    Negative,
    Positive,
    StrictlyPositive,
    #[default] Unknown,
}

impl Polarity {
    pub fn invert(&self) -> Polarity {
        match self {
            Polarity::Negative => Polarity::Positive,
            Polarity::Positive | Polarity::StrictlyPositive => Polarity::Negative,
            other => *other,
        }
    }
    
    pub fn is_positive(&self) -> bool {
        matches!(self, Polarity::Positive | Polarity::StrictlyPositive)
    }
    
    pub fn is_strictly_positive(&self) -> bool {
        matches!(self, Polarity::StrictlyPositive)
    }
    
    pub fn is_negative(&self) -> bool {
        matches!(self, Polarity::Negative)
    }
    
    pub fn is_unknown(&self) -> bool {
        matches!(self, Polarity::Unknown)
    }
}

impl PrettyPrintable for Polarity {
    fn to_pp_element(&self, _detail: bool) -> PPElement {
        PPElement::text(format!("Polarity::{}", match self {
            Polarity::Negative => "Negative",
            Polarity::Positive => "Positive",
            Polarity::StrictlyPositive => "StrictlyPositive",
            Polarity::Unknown => "Unknown",
        }).to_string())
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Polarized<T: PrettyPrintable + Clone + PartialEq + Eq> {
    pub polarity: Polarity,
    pub value: T,
}

impl<T: PrettyPrintable + Clone + PartialEq + Eq> Polarized<T> {
    pub fn new(polarity: Polarity, value: T) -> Self {
        Polarized { polarity, value }
    }
    
    pub fn unwrap(self) -> T {
        self.value
    }
}

impl<T: PrettyPrintable + Clone + PartialEq + Eq> PrettyPrintable for Polarized<T> {
    fn to_pp_element(&self, detail: bool) -> PPElement {
        PPElement::group(vec![
            PPElement::text("Polarized(".to_string()),
            PPElement::break_elem(1, 4, false),
            self.polarity.to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text("|".to_string()),
            PPElement::break_elem(1, 4, false),
            self.value.to_pp_element(detail),
            PPElement::break_elem(1, 0, false),
            PPElement::text(")".to_string()),
        ], BreakType::Consistent, 0)
    }
    
    fn requires_parens(&self, _detail: bool) -> bool {
        false
    }
}

impl<T: PrettyPrintable + Clone + PartialEq + Eq> From<(Polarity, T)> for Polarized<T> {
    fn from(tuple: (Polarity, T)) -> Self {
        Polarized::new(tuple.0, tuple.1)
    }
}

impl<T: PrettyPrintable + Clone + PartialEq + Eq> From<Polarized<T>> for (Polarity, T) {
    fn from(polarised: Polarized<T>) -> Self {
        (polarised.polarity, polarised.value)
    }
}