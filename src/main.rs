use std::{
    convert::{TryFrom, TryInto},
    io::Read,
    str::FromStr,
    sync::{Arc, Condvar, Mutex},
    time::{Duration, Instant},
};

use clap::{AppSettings, Clap};
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use num_bigint::BigUint;
use while_lang::{compile, Evaluator, Prog};

#[derive(Clap, Debug, Clone)]
#[clap(version, about)]
#[clap(setting = AppSettings::ColoredHelp)]
#[clap(setting = AppSettings::SubcommandRequiredElseHelp)]
struct Args {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap, Debug, Clone)]
enum SubCommand {
    #[clap(about = "Check if a given program is well-formed without running it")]
    Check { input_file: String },

    #[clap(about = "Run a given program")]
    Run { input_file: String },

    #[clap(about = "Evaluate a program one instruction at a time")]
    Debug { input_file: String },

    #[clap(about = "Translate a program into a pure WHILE program (experimental)")]
    Rewrite { input_file: String },

    #[clap(about = "Translate a program into its Gödel number")]
    Encode { input_file: String },

    #[clap(about = "Translate a Gödel number into a while program")]
    Decode { input_file: String },
}

fn main() {
    let args: Args = Args::parse();

    let mut files = SimpleFiles::new();
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let (prog, code) = match args.subcmd.clone() {
        SubCommand::Check { input_file }
        | SubCommand::Run { input_file }
        | SubCommand::Debug { input_file }
        | SubCommand::Rewrite { input_file }
        | SubCommand::Encode { input_file } => {
            let code = std::fs::read_to_string(&input_file).unwrap();
            let file_id = files.add(&input_file, &code);

            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(errors) => {
                    for error in errors {
                        term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    }
                    return;
                }
            };

            for lint in prog.check_loops(file_id) {
                term::emit(&mut writer.lock(), &config, &files, &lint).unwrap();
            }

            (prog, code)
        }

        SubCommand::Decode { input_file } => {
            let mut text = std::fs::read_to_string(&input_file).unwrap();
            text.retain(|c| matches!(c, '0'..='9'));

            let num = BigUint::from_str(&text).unwrap();
            let prog = Prog::try_from(num).unwrap().inline_blocks();
            println!("{}", prog);

            return;
        }
    };

    match args.subcmd {
        SubCommand::Check { .. } => (),

        SubCommand::Run { .. } => {
            #[derive(Clone)]
            struct Stats {
                done: bool,
                total_insts: u64,
                epoch_insts: u64,
                epoch_len: Duration,
            }

            let mut ev = Evaluator::new(prog);
            let stats_handle = Arc::new((Mutex::new(None), Condvar::new()));

            let worker = {
                let stats_handle = Arc::clone(&stats_handle);
                std::thread::spawn(move || {
                    let mut epoch = Instant::now();
                    let mut total_insts: u64 = 0;
                    let mut epoch_insts: u64 = 0;

                    loop {
                        let done = !ev.step();
                        epoch_insts += 1;

                        let epoch_len = epoch.elapsed();
                        if done || epoch_len > Duration::from_secs(1) {
                            epoch = Instant::now();
                            total_insts += epoch_insts;

                            let (stats, cvar) = &*stats_handle;
                            *stats.lock().unwrap() = Some(Stats {
                                done,
                                total_insts,
                                epoch_insts,
                                epoch_len,
                            });

                            cvar.notify_all();
                            epoch_insts = 0;
                        }

                        if done {
                            break ev;
                        }
                    }
                })
            };

            let start = Instant::now();
            let (stats, cvar) = &*stats_handle;
            let mut guard = stats.lock().unwrap();

            loop {
                let stats = match guard.take() {
                    Some(s) => s,
                    None => {
                        guard = cvar.wait(guard).unwrap();
                        continue;
                    }
                };

                let elapsed = start.elapsed();
                println!(
                    "..running, time {:?}, total {}, rate {:.3}#/s",
                    elapsed,
                    stats.total_insts,
                    stats.epoch_insts as f64 / stats.epoch_len.as_secs_f64()
                );

                if stats.done {
                    break;
                }
            }

            let ev = worker.join().unwrap();

            println!("{{");
            for (key, value) in ev.base.state.into_iter() {
                if value.is_zero() {
                    continue;
                }

                println!("    {} => {}", key, value);
            }
            println!("}}");
        }

        SubCommand::Debug { .. } => {
            let mut ev = Evaluator::new(prog);
            let mut wait_for_input = true;

            while ev.step() {
                ev.debug_print(&code, 7);

                if wait_for_input {
                    let mut stdin = std::io::stdin();
                    let mut buffer = [0u8; 4];
                    stdin.read(&mut buffer).unwrap();

                    if buffer.starts_with(b"q") {
                        return;
                    }

                    if buffer.starts_with(b"r") {
                        wait_for_input = false;
                    }
                }

                // clear the screen
                print!("\x1B[2J\x1B[1;1H");
            }

            println!("{{");
            for (key, value) in ev.base.state.into_iter() {
                if value.is_zero() {
                    continue;
                }

                println!("    {} => {}", key, value);
            }
            println!("}}");
        }

        SubCommand::Rewrite { .. } => {
            let prog = prog
                .inline_functions()
                .shortcut_tuples()
                .for_to_while()
                .remove_unused()
                .expand_copy()
                .inline_blocks();

            println!("{}", prog);
        }

        SubCommand::Encode { .. } => {
            let num: BigUint = prog
                .inline_functions()
                .shortcut_tuples()
                .for_to_while()
                .remove_unused()
                .expand_copy()
                .inline_blocks()
                .reindex_vars()
                .try_into()
                .unwrap();

            println!("{}", num);
        }

        SubCommand::Decode { .. } => unreachable!(),
    }
}
