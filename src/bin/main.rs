
use lib::utils::pretty_printer::PrettyPrinter;
use lib::pretty_print;

pub fn main() {
    let mut pp = PrettyPrinter::new(80);
    let res = pretty_print!(pp, <| "macro_rules!" -- "pretty_print_object" -- "{" |> <<4| <| "(" -- "$printer:expr,"
        -- "$($item:tt)*" -- ")" -- "=>" -- "{" |> <<4| "$(pretty_print_tokens!" <| "(" <<4| "$printer," -- "$item" |>> ");" |> |>> "};" |>> "}" );
    println!("{}", res);
}
