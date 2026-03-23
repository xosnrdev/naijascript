use std::io::{self, Read, Write};
use std::process::ExitCode;
use std::time::Duration;
use std::{env, thread};

fn usage() -> ExitCode {
    eprintln!(
        "usage: process_helper <command> [args...]
commands:
  exit <code>
  echo-stdout <text>
  echo-stderr <text>
  echo-both <stdout> <stderr>
  cat-stdin
  print-env <key>
  print-cwd
  print-args <arg>...
  sleep-ms <ms>
  emit-bytes <hex>"
    );
    ExitCode::from(64)
}

fn parse_u8_hex(hex: &str) -> Result<Vec<u8>, ()> {
    if !hex.len().is_multiple_of(2) {
        return Err(());
    }

    let mut out = Vec::with_capacity(hex.len() / 2);
    let bytes = hex.as_bytes();
    let mut idx = 0;
    while idx < bytes.len() {
        let hi = (bytes[idx] as char).to_digit(16).ok_or(())?;
        let lo = (bytes[idx + 1] as char).to_digit(16).ok_or(())?;
        let byte = u8::try_from((hi << 4) | lo).map_err(|_| ())?;
        out.push(byte);
        idx += 2;
    }
    Ok(out)
}

fn main() -> ExitCode {
    let mut args = env::args();
    let _program = args.next();
    let Some(command) = args.next() else {
        return usage();
    };

    match command.as_str() {
        "exit" => {
            let Some(code) = args.next() else {
                return usage();
            };
            let Ok(code) = code.parse::<u8>() else {
                return usage();
            };
            ExitCode::from(code)
        }
        "echo-stdout" => {
            let Some(text) = args.next() else {
                return usage();
            };
            print!("{text}");
            ExitCode::SUCCESS
        }
        "echo-stderr" => {
            let Some(text) = args.next() else {
                return usage();
            };
            eprint!("{text}");
            ExitCode::SUCCESS
        }
        "echo-both" => {
            let (Some(stdout), Some(stderr)) = (args.next(), args.next()) else {
                return usage();
            };
            print!("{stdout}");
            eprint!("{stderr}");
            ExitCode::SUCCESS
        }
        "cat-stdin" => {
            let mut input = String::new();
            if io::stdin().read_to_string(&mut input).is_err() {
                return ExitCode::from(1);
            }
            print!("{input}");
            ExitCode::SUCCESS
        }
        "print-env" => {
            let Some(key) = args.next() else {
                return usage();
            };
            if let Ok(value) = env::var(&key) {
                print!("{value}");
            }
            ExitCode::SUCCESS
        }
        "print-cwd" => {
            let Ok(cwd) = env::current_dir() else {
                return ExitCode::from(1);
            };
            print!("{}", cwd.display());
            ExitCode::SUCCESS
        }
        "print-args" => {
            let mut first = true;
            for arg in args {
                if !first {
                    println!();
                }
                first = false;
                print!("{arg}");
            }
            ExitCode::SUCCESS
        }
        "sleep-ms" => {
            let Some(ms) = args.next() else {
                return usage();
            };
            let Ok(ms) = ms.parse::<u64>() else {
                return usage();
            };
            thread::sleep(Duration::from_millis(ms));
            ExitCode::SUCCESS
        }
        "emit-bytes" => {
            let Some(hex) = args.next() else {
                return usage();
            };
            let Ok(bytes) = parse_u8_hex(&hex) else {
                return usage();
            };
            if io::stdout().write_all(&bytes).is_err() {
                return ExitCode::from(1);
            }
            ExitCode::SUCCESS
        }
        _ => usage(),
    }
}
