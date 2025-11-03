mod opts;
mod test_script;
mod concat_reader;
mod test_case;

use chrono::{DateTime, Utc};
use opts::Opts;
use std::process::Command;
use structopt::StructOpt;
use test_case::TestCase;

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
        Err(_) => "unknown timestamp".to_string(),
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
    println!("found {} tests...", test_files.len());

    let test_count = test_files.len();
    let mut ok_count = 0;
    let mut error_count = 0;

    for test_file in test_files {
        if !test_file.run(&opts) {
            error_count += 1;
            if !opts.error_continue {  
                break;
            }
        } else {
            ok_count += 1;
        }
    }
    
    let skipped_count = test_count - (ok_count + error_count);
        println!("OK: {ok_count}, ERRORS: {error_count}, SKIPPED: {skipped_count}");
    
    Ok(())
}
