use std::{
    env, fs,
    io::{self, Write},
    process::ExitCode,
};

use scanner::Scanner;

mod errors;
mod scanner;
mod token;

fn main() -> ExitCode {
    match env::args().len() {
        1 => run_prompt(),
        2 => run_file(env::args().nth(1).unwrap().as_str()),
        _ => {
            eprintln!("Usage: rlox [script]");
            ExitCode::from(64)
        }
    }
}

/// Run an rlox script located in `filename`
fn run_file(filename: &str) -> ExitCode {
    println!("Running {}", filename);
    let file_contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(_) => return ExitCode::FAILURE,
    };

    println!("{}", file_contents);

    ExitCode::SUCCESS
}

/// Run rlox in interactive mode
fn run_prompt() -> ExitCode {
    println!("rlox v0.0.1");
    loop {
        print!("> ");

        if let Err(_) = io::stdout().flush() {
            eprint!("IO Error: Unable to flush stdout");
            return ExitCode::FAILURE;
        }

        let mut raw_input = String::new();

        if let Err(_) = io::stdin().read_line(&mut raw_input) {
            eprintln!("Unable to read input!");
            continue;
        }

        let trimmed_input = raw_input.trim_end();

        if trimmed_input.is_empty() {
            break;
        }

        let scanner = Scanner::new(trimmed_input);

        let (tokens, errors) = scanner.scan_tokens();

        if !errors.is_empty() {
            println!("Found {} errors while scanning", errors.len());
        }

        for token in tokens {
            println!("{:?}", token);
        }
    }

    ExitCode::SUCCESS
}

fn error(line: u32, message: &str) {
    report(errors::Error {
        line,
        position_in_string: "".to_owned(),
        message: message.to_owned(),
    });
}

fn report(error: errors::Error) {
    let errors::Error {
        line,
        position_in_string,
        message,
    } = error;
    eprintln!("[line {}] Error {}: {}", line, position_in_string, message);
}
