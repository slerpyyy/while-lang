use std::{
    io::Read,
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
use while_lang::{compile, Evaluator};

#[derive(Clap, Debug, Clone)]
#[clap(setting = AppSettings::ColoredHelp)]
#[clap(setting = AppSettings::SubcommandRequiredElseHelp)]
struct Args {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap, Debug, Clone)]
enum SubCommand {
    Check { input_file: String },
    Run { input_file: String },
    Debug { input_file: String },
    Rewrite { input_file: String },
}

fn main() {
    let args: Args = Args::parse();

    let mut files = SimpleFiles::new();
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let input_file = match args.subcmd.clone() {
        SubCommand::Check { input_file } => input_file,
        SubCommand::Run { input_file } => input_file,
        SubCommand::Debug { input_file } => input_file,
        SubCommand::Rewrite { input_file } => input_file,
    };

    let code = std::fs::read_to_string(&input_file).unwrap();
    let file_id = files.add(&input_file, &code);

    match args.subcmd {
        SubCommand::Check { .. } => {
            if let Err(error) = compile(&code, file_id) {
                term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                return;
            }
        }
        SubCommand::Run { .. } => {
            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(error) => {
                    term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    return;
                }
            };

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
            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(error) => {
                    term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    return;
                }
            };

            let mut ev = Evaluator::new(prog);
            let mut stdin = std::io::stdin();

            while ev.step() {
                let _ = std::process::Command::new("cmd.exe")
                    .arg("/c")
                    .arg("cls")
                    .status();
                ev.debug_print(&code, 7);

                stdin.read(&mut [0u8; 4]).unwrap();
            }

            let _ = std::process::Command::new("cmd.exe")
                .arg("/c")
                .arg("cls")
                .status();
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
            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(error) => {
                    term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    return;
                }
            };

            let prog = prog
                .inline_functions()
                .shortcut_tuples()
                .for_to_while()
                .remove_unused()
                .expand_copy()
                .inline_blocks();

            println!("{}", prog);
        }
    }
}
