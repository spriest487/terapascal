mod opts;
mod test_script;
mod concat_reader;
mod runner;
mod test_case;
mod build;
mod error;

use crate::build::check_environment;
use crate::runner::run;
use crate::test_case::TestCase;
use opts::Opts;
use structopt::StructOpt;

fn main() -> Result<(), i32> {
    let opts = Opts::from_args();

    if !check_environment(&opts) {
        return Err(2);
    }

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
