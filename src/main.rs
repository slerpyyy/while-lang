use clap::{AppSettings, Clap};
use codespan_reporting::{files::SimpleFiles, term::{self, termcolor::{ColorChoice, StandardStream}}};
use while_lang::{Evaluator, compile};

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
#[clap(setting = AppSettings::SubcommandRequiredElseHelp)]
struct Args {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Check {
        input_file: String
    },
    Run {
        input_file: String
    },
    Debug {
        input_file: String
    },
    Rewrite {
        input_file: String
    },
}

fn main() {
    let args: Args = Args::parse();

    let mut files = SimpleFiles::new();
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    match args.subcmd {
        SubCommand::Check { input_file } => {
            let code = std::fs::read_to_string(&input_file).unwrap();
            let file_id = files.add(&input_file, &code);

            if let Err(error) = compile(&code, file_id) {
                term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                return;
            }
        }
        SubCommand::Run { input_file } => {
            let code = std::fs::read_to_string(&input_file).unwrap();
            let file_id = files.add(&input_file, &code);

            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(error) => {
                    term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    return;
                }
            };

            let mut ev = Evaluator::new(prog);
            ev.run();

            println!("{{");
            for (key, value) in ev.state.into_iter() {
                println!("    {} => {}", key, value);
            }
            println!("}}");
        }
        SubCommand::Debug { input_file } => {
            let code = std::fs::read_to_string(&input_file).unwrap();
            let file_id = files.add(&input_file, &code);

            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(error) => {
                    term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    return;
                }
            };

            let mut ev = Evaluator::new(prog);
            while ev.step() {
                let _ = std::process::Command::new("cmd.exe").arg("/c").arg("cls").status();
                ev.debug_print(&code, 7);
                let _ = std::process::Command::new("cmd.exe").arg("/c").arg("pause").status();
            }

            let _ = std::process::Command::new("cmd.exe").arg("/c").arg("cls").status();
            println!("{{");
            for (key, value) in ev.state.into_iter() {
                println!("    {} => {}", key, value);
            }
            println!("}}");
        }
        SubCommand::Rewrite { input_file } => {
            let code = std::fs::read_to_string(&input_file).unwrap();
            let file_id = files.add(&input_file, &code);

            let prog = match compile(&code, file_id) {
                Ok(s) => s,
                Err(error) => {
                    term::emit(&mut writer.lock(), &config, &files, &error).unwrap();
                    return;
                }
            };

            let prog = prog
                .inline_functions()
                .for_to_while()
                .unused_sets()
                .expand_copy()
                .inline_blocks();

            println!("{}", prog);
        },
    }
}
