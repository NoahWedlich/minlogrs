#![warn(clippy::all)]

pub fn say_hello() {
    println!("Hello!")
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
