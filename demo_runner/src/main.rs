mod opts;
mod test_script;
mod build;
mod test_runner;
mod error;
mod util;

use crate::build::*;
use crate::opts::Opts;
use crate::test_runner::*;
use rayon::iter::*;
use std::sync;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use structopt::StructOpt;

fn main() -> Result<(), i32> {
    let opts = Opts::from_args();

    if !check_environment(&opts) {
        return Err(2);
    }

    if opts.verbose {
        println!("source path: {}", opts.search_path.display());
        println!("target path: {}", opts.target_path.display());
        println!("compiler path: {}", opts.compiler.display());
    }

    let cases: Vec<_> = TestCase::find_at_path(&opts.search_path)
        .into_iter()
        .enumerate()
        .collect();

    let (send_test_output, recv_test_output) = sync::mpsc::channel::<TestOutput>();

    let case_count = cases.len();

    let mut failed_paths = Vec::new();
    let mut skipped_count = 0;

    let output_thread = thread::spawn(move || {
        let mut outputs = Vec::with_capacity(case_count);
        outputs.resize_with(case_count, || None);

        let mut next_index = 0;

        while let Ok(new_output) = recv_test_output.recv() {
            let index = new_output.index;
            outputs[index] = Some(new_output);

            while next_index < case_count
                && let Some(output) = outputs[next_index].take()
            {
                next_index += 1;

                if output.status.is_skipped() {
                    skipped_count += 1;
                    continue;
                }

                println!();
                println!("TEST: {} ({}/{})", output.path.display(), output.index + 1, case_count);
                if !output.log.is_empty() {
                    println!("{}", &output.log);
                }

                match output.status {
                    TestStatus::OK => {
                        println!("OK");
                    }

                    TestStatus::Failed(reason) => {
                        print!("FAILED: ");
                        match reason {
                            FailureReason::MissingOut(expected) => {
                                println!("expected output '{expected}'");
                            },
                            FailureReason::UnexpectedErr(output) => {
                                println!("unexpected error output '{output}'");
                            }
                        }
                        failed_paths.push(output.path);
                    }

                    TestStatus::Error(err) => {
                        println!("ABORTED: {err}");
                        failed_paths.push(output.path);
                    }

                    TestStatus::Skipped | TestStatus::Running => {
                        unreachable!()
                    }
                }
            }
        }

        eprintln!();

        let failed_count = failed_paths.len();
        let ok_count = case_count - (skipped_count + failed_count);
        println!("OK: {ok_count} / FAILED: {failed_count} / SKIPPED: {skipped_count}");

        if failed_count > 0 {
            println!("Failed:");

            for failed_path in failed_paths {
                println!("{}", failed_path.display());
            }
        }
    });

    let skip_token = Arc::new(Mutex::new(false));

    cases
        .into_par_iter()
        .map(|(i, case)| {
            if let Ok(skip) = skip_token.try_lock() && *skip {
                return TestOutput::skipped(case.path, i);
            }

            let runner = TestRunner::new(&opts, i, case);
            let output = runner.run();

            if !output.status.is_ok()
                && !opts.error_continue
                && let Ok(mut skip) = skip_token.try_lock()
            {
                *skip = true;
            }

            output
        })
        .for_each(|output| {
            send_test_output
                .send(output)
                .expect("test output channel was unexpectedly closed");
        });

    drop(send_test_output);

    if let Err(..) = output_thread.join() {
        eprintln!("joining output thread failed!");
        return Err(3);
    }

    Ok(())
}