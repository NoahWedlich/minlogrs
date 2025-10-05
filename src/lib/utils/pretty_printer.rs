use core::panic;
use std::collections::VecDeque;

// A simple pretty-printer based on the algorithm described in "Pretty Printing" by Derek C. Oppen

pub trait PrettyPrintable {
    fn pretty_print(&self, printer: &mut PrettyPrinter, detail: bool);

    fn requires_parens(&self, _detail: bool) -> bool { false }
    
    fn open_paren(&self) -> &'static str { "{" }
    
    fn close_paren(&self) -> &'static str { "}" }
}

#[derive(Clone, Copy, Debug)]
pub enum BreakType {
    CONSISTENT,
    INCONSISTENT,
}

#[derive(Clone, Copy, Debug)]
pub enum Token<'a> {
    String{content: &'a str},
    Break{spaces: u8, offset: u8},
    Begin{indent: u8, break_type: BreakType},
    End,
    EOF,
}

#[derive(Clone, Copy, Debug)]
enum PrintBreak {
    FITS,
    CONSISTENT,
    INCONSISTENT,
}

#[derive(Clone, Copy, Debug)]
struct PrintFrame {
    offset: u8,
    break_type: PrintBreak
}

pub struct PrettyPrinter<'a> {
    line_width: u8,
    space_left: u8,
    
    left_index: u16,
    right_index: u16,

    tokens: Vec<Token<'a>>,
    sizes: Vec<i16>,

    left_total: u16,
    right_total: u16,
    
    scan_queue: VecDeque<u16>,
    print_stack: Vec<PrintFrame>,

    output: String,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(line_width: u8) -> PrettyPrinter<'a> {
        PrettyPrinter {
            line_width: line_width,
            space_left: line_width,
            left_index: 0,
            right_index: 0,
            tokens: vec![Token::EOF; 3 * line_width as usize],
            sizes: vec![0; 3 * line_width as usize],
            left_total: 0,
            right_total: 0,
            scan_queue: VecDeque::with_capacity(3 * line_width as usize),
            print_stack: Vec::with_capacity(64),
            output: String::new(),
        }
    }
    
    pub fn render(&mut self, object: &impl PrettyPrintable, detail: bool) -> &str {
        self.clear();
        self.add_token(Token::Begin{indent: 0, break_type: BreakType::INCONSISTENT});
        
        if object.requires_parens(detail) {
            self.add_token(Token::String{content: object.open_paren()});
            object.pretty_print(self, detail);
            self.add_token(Token::String{content: object.close_paren()});
        } else {
            object.pretty_print(self, detail);
        }
        
        self.add_token(Token::End);
        self.add_token(Token::EOF);
        self.finish()
    }
    
    pub fn print(&mut self, object: &impl PrettyPrintable, detail: bool) {
        println!("{}", self.render(object, detail));
    }

    pub fn add_string(&mut self, string: &'a str) {
        self.add_token(Token::String{content: string});
    }

    pub fn add_linebreak(&mut self, count: u8) {
        for _ in 0..count {
            self.add_token(Token::Break{spaces: self.line_width, offset: 0});
        }
    }

    pub fn add_indent(&mut self, spaces: u8) {
        self.add_token(Token::Break{spaces, offset: spaces});
    }

    pub fn add_break(&mut self, spaces: u8, offset: u8) {
        self.add_token(Token::Break{spaces, offset});
    }

    pub fn begin(&mut self, indent: u8, break_type: BreakType) {
        self.add_token(Token::Begin{indent, break_type});
    }

    pub fn end(&mut self) {
        self.add_token(Token::End);
    }

    pub fn finish(&mut self) -> &str {
        &self.output
    }

    pub fn clear(&mut self) {
        self.space_left = self.line_width;
        self.left_index = 0;
        self.right_index = 0;
        self.tokens.clear();
        self.sizes.clear();
        self.left_total = 0;
        self.right_total = 0;
        self.scan_queue.clear();
        self.print_stack.clear();
        self.output.clear();
    }
    
    pub fn add_token(&mut self, token: Token<'a>) {
        match token {
            Token::String{content} => {
                if self.scan_queue.is_empty() {
                    self.process_token(&token, content.len() as u8);
                } else {
                    self.advance_right();
                    self.tokens[self.right_index as usize] = token;
                    self.sizes[self.right_index as usize] = content.len() as i16;
                    self.right_total += content.len() as u16;
                    self.check_stream();
                }
            },
            Token::Break{spaces, offset: _} => {
                if self.scan_queue.is_empty() {
                    self.left_index = 0;
                    self.right_index = 0;
                    self.left_total = 1;
                    self.right_total = 1;
                } else {
                    self.advance_right();
                }
                
                self.check_queue(0);
                self.scan_queue.push_back(self.right_index);
                self.tokens[self.right_index as usize] = token;

                self.sizes[self.right_index as usize] = -(self.right_total as i16);
                self.right_total += spaces as u16;
            },
            Token::Begin{indent: _, break_type: _} => {
                if self.scan_queue.is_empty() {
                    self.left_index = 0;
                    self.right_index = 0;
                    self.left_total = 1;
                    self.right_total = 1;
                } else {
                    self.advance_right();
                }
                
                self.tokens[self.right_index as usize] = token;
                self.sizes[self.right_index as usize] = -(self.right_total as i16);
                self.scan_queue.push_back(self.right_index);
            },
            Token::End => {
                if self.scan_queue.is_empty() {
                    self.process_token(&token, 0);
                } else {
                    self.advance_right();
                    self.tokens[self.right_index as usize] = token;
                    self.sizes[self.right_index as usize] = -(self.right_total as i16);
                    self.scan_queue.push_back(self.right_index);
                }
            },
            Token::EOF => {
                if !self.scan_queue.is_empty() {
                    self.check_queue(0);
                    self.advance_left();
                }
                
                if !self.scan_queue.is_empty() {
                    panic!("PrettyPrinter: scan queue left in an unfinished state at end of input: \n\t{:?}", self.scan_queue);
                }
            }
        }
    }
    
    fn process_token(&mut self, token: &Token<'a>, length: u8) {
        match token {
            Token::String{content} => {
                if length > self.space_left {
                    panic!("PrettyPrinter: string length exceeds line width");
                }
                self.space_left -= length;
                self.output.push_str(content);
            },
            Token::Break{spaces, offset} => {
                match self.print_stack.last() {
                    Some(frame) => match frame.break_type {
                        PrintBreak::FITS => {
                            self.space_left -= spaces;
                            self.output.push_str(&" ".repeat(*spaces as usize));
                        },
                        PrintBreak::CONSISTENT => {
                            self.space_left = frame.offset - offset;
                            self.output.push('\n');
                            self.output.push_str(&" ".repeat((self.line_width - self.space_left) as usize));
                        },
                        PrintBreak::INCONSISTENT => {
                            if length > self.space_left {
                                self.space_left = frame.offset - offset;
                                self.output.push('\n');
                                self.output.push_str(&" ".repeat((self.line_width - self.space_left) as usize));
                            } else {
                                self.space_left -= spaces;
                                self.output.push_str(&" ".repeat(*spaces as usize));
                            }
                        }
                    },
                    None => {
                        panic!("PrettyPrinter: break token with no print stack");
                    }
                }
            },
            Token::Begin{indent, break_type} => {
                if length > self.space_left {
                    self.print_stack.push(PrintFrame{
                        offset: self.space_left - indent,
                        break_type: match break_type {
                            BreakType::CONSISTENT => PrintBreak::CONSISTENT,
                            BreakType::INCONSISTENT => PrintBreak::INCONSISTENT,
                        }
                    });
                } else {
                    self.print_stack.push(PrintFrame{
                        offset: 0,
                        break_type: PrintBreak::FITS
                    });
                }
            },
            Token::End => {
                self.print_stack.pop();
            },
            Token::EOF => {
                panic!("PrettyPrinter: unexpected EOF token in process_token");
            }
        }
    }
    
    fn check_stream(&mut self) {
        if self.right_total - self.left_total > self.space_left as u16 {
            if !self.scan_queue.is_empty() {
                if self.left_index == *self.scan_queue.front().unwrap() {
                    self.sizes[self.scan_queue.pop_front().unwrap() as usize] = i16::MAX;
                }
                
                self.advance_left();
                
                if self.left_index != self.right_index {
                    self.check_stream();
                }
            }
        }
    }
    
    fn check_queue(&mut self, offset: u16) {
        if !self.scan_queue.is_empty() {
            let index: u16 = *self.scan_queue.back().unwrap();
            
            match self.tokens[index as usize] {
                Token::Begin{indent: _, break_type: _} => {
                    if offset > 0 {
                        self.sizes[self.scan_queue.pop_back().unwrap() as usize] =
                            self.sizes[index as usize] + self.right_total as i16;
                        self.check_queue(offset - 1);
                    }
                },
                Token::End => {
                    self.sizes[self.scan_queue.pop_back().unwrap() as usize] = 1;
                    self.check_queue(offset + 1);
                },
                _ => {
                    self.sizes[self.scan_queue.pop_back().unwrap() as usize] =
                        self.sizes[index as usize] + self.right_total as i16;
                    if offset > 0 {
                        self.check_queue(offset);
                    }
                }
            }   
        }
    }
    
    fn advance_left(&mut self) {
        let token: Token<'a> = self.tokens[self.left_index as usize];
        let size: i16 = self.sizes[self.left_index as usize];
        
        if size >= 0 {
            self.process_token(&token, size as u8);
            
            match token {
                Token::String{content} => {
                    self.left_total += content.len() as u16;
                },
                Token::Break{spaces, offset: _} => {
                    self.left_total += spaces as u16;
                },
                _ => {}
            }
            
            if self.left_index != self.right_index {
                self.left_index = (self.left_index + 1) % self.tokens.capacity() as u16;
                self.advance_left();
            }
        }
    }
    
    fn advance_right(&mut self) {
        self.right_index = (self.right_index + 1) % self.tokens.capacity() as u16;
        if self.right_index == self.left_index {
            panic!("PrettyPrinter: token buffer overflow");
        }
    }
}

#[macro_export]
macro_rules! pretty_print_tokens {
    ($printer:expr, ) => {()};
    
    ($printer:expr, $object:ident $($rest:tt)*) => {
        {
            if $object.requires_parens(false) {
                $printer.add_token($crate::utils::pretty_printer::Token::String{content: $object.open_paren()});
                $object.pretty_print($printer, false);
                $printer.add_token($crate::utils::pretty_printer::Token::String{content: $object.close_paren()});
            } else {
                $object.pretty_print($printer, false);
            }
                
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, # $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Break{spaces: u8::MAX, offset: 0});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, <| $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Begin{indent: 0,
                break_type: $crate::utils::pretty_printer::BreakType::INCONSISTENT});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, <$indent:literal| $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Begin{indent: $indent,
                break_type: $crate::utils::pretty_printer::BreakType::INCONSISTENT});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, <$indent:ident| $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Begin{indent: $indent,
                break_type: $crate::utils::pretty_printer::BreakType::INCONSISTENT});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, <<| $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Begin{indent: 0,
                break_type: $crate::utils::pretty_printer::BreakType::CONSISTENT});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, <<$indent:literal| $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Begin{indent: $indent,
                break_type: $crate::utils::pretty_printer::BreakType::CONSISTENT});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, <<$indent:ident| $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Begin{indent: $indent,
                break_type: $crate::utils::pretty_printer::BreakType::CONSISTENT});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, |> $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::End);
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, |>> $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::End);
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, -- $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Break{spaces: 1, offset: 0});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, -$spaces:literal- $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Break{spaces: $spaces, offset: 0});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, -$spaces:ident- $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Break{spaces: $spaces, offset: 0});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, -$spaces:literal|$offset:literal- $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Break{spaces: $spaces, offset: $offset});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, -$spaces:ident|$offset:ident- $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::Break{spaces: $spaces, offset: $offset});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, $string:literal $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::String{content: $string});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, $string:ident $($rest:tt)*) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::String{content: $string});
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
}

#[macro_export]
macro_rules! pretty_print_object {
    ($printer:expr, ) => {};
    
    ($printer:expr, $object:ident $($rest:tt)*) => {
        {
            if $object.requires_parens(true) {
                $printer.add_token($crate::utils::pretty_printer::Token::String{content: $object.open_paren()});
                $object.pretty_print($printer, true);
                $printer.add_token($crate::utils::pretty_printer::Token::String{content: $object.close_paren()});
            } else {
                $object.pretty_print($printer, true);
            }
            
            $crate::pretty_print_tokens!($printer, $($rest)*)
        }
    };
    
    ($printer:expr, $($rest:tt)*) => {
        $crate::pretty_print_tokens!($printer, $($rest)*)
    }
}

#[macro_export]
macro_rules! pretty_print {
    ($printer:expr, ) => {
        {
            $printer.add_token($crate::utils::pretty_printer::Token::EOF);
            $printer.finish()
        }
    };
    
    ($printer:expr, $($item:tt)*) => {
        {

            $crate::pretty_print_tokens!($printer, <|);

            $crate::pretty_print_object!($printer, $($item)*);

            $crate::pretty_print_tokens!($printer, |>);

            $printer.add_token($crate::utils::pretty_printer::Token::EOF);
            $printer.finish()
        }
    };
}