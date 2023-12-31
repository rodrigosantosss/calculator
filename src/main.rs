mod compute;
mod parser;
mod tokenizer;

use compute::compute_tree;
use parser::parse;
use rust_decimal::RoundingStrategy;
use tokenizer::tokenize;

fn main() {
    let mut input = String::new();
    let mut ans = None;

    loop {
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => (),
            Err(_) => {
                eprintln!("Error reading line from standard input.");
                std::process::exit(1);
            }
        };

        let tokens = tokenize(input.clone());
        input.clear();

        let tokens = match tokens {
            Some(tokens) => tokens,
            None => {
                println!("Invalid token.");
                continue;
            }
        };

        let tree = match parse(tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("Syntax error: {err}");
                continue;
            }
        };

        ans = match compute_tree(tree, ans) {
            Ok(x) => {
                println!(
                    "{}",
                    x.round_dp_with_strategy(10, RoundingStrategy::MidpointAwayFromZero)
                );
                Some(x)
            }
            Err(err) => {
                println!("Math error: {err}");
                continue;
            }
        };
    }
}
