mod opts;
mod test_script;
mod concat_reader;
mod runner;
mod test_case;
mod build;

use crate::runner::run;
use crate::test_case::TestCase;
use chrono::DateTime;
use chrono::Utc;
use opts::Opts;
use std::process::Command;
use structopt::StructOpt;

fn main() -> Result<(), i32> {
    let opts = Opts::from_args();

    let exists = opts.compiler.exists();
    if !exists {
        eprintln!("frontend not found! expected at {}", opts.compiler.display());
        return Err(1);
    }

    println!("using frontend: {}", opts.compiler.display());

    let timestamp = match opts.compiler.metadata().and_then(|metadata| metadata.modified()) {
        Ok(modified) => {
            let modified_date = DateTime::<Utc>::from(modified);
            format!("{}", modified_date.format("%Y-%m-%d %H:%M"))
        }
        Err(err) => {
            eprintln!("unable to read compiler timestamp: {}", err);
            "unknown timestamp".to_string()
        },
    };

    let version_check_out = Command::new(opts.compiler.clone())
        .arg("--version")
        .output()
        .map_err(|err| {
            eprintln!("version check failed: {}", err);
            1
        })?;

    println!("{} ({})", String::from_utf8(version_check_out.stdout).unwrap().trim(), timestamp);

    let test_files = TestCase::find_at_path(&opts.search_path);

    if opts.verbose {
        println!("found {} tests:", test_files.len());
        for test_case in &test_files {
            println!("\t{}", test_case.path.display());
        }
    }

    let mut ok_count = 0;
    let mut error_count = 0;
    
    let mut failed_files = Vec::new();

    let test_count = test_files.len();
    for (i, test_file) in test_files.into_iter().enumerate() {
        println!("RUNNING: {} ({}/{})", test_file.path.display(), i + 1, test_count);

        if !run(&test_file, &opts) {
            error_count += 1;
            if !opts.error_continue {
                break;
            }
            
            failed_files.push(test_file.path);
        } else {
            ok_count += 1;
        }
    }
    
    let skipped_count = test_count - (ok_count + error_count);
        println!("OK: {ok_count}, ERRORS: {error_count}, SKIPPED: {skipped_count}");
    
    if !failed_files.is_empty() {
        println!("FAILURES:");
        for path in failed_files {
            println!("{}", path.display());
        }
    }
    
    Ok(())
}
