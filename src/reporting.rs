use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::diagnostic::LabelStyle;
use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::Error as FileError;
use codespan_reporting::files::Files;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor;
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use terapascal_common::fs::DefaultFilesystem;
use terapascal_common::fs::Filesystem;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;

type CodeMap = SimpleFiles<String, String>;

fn output_to_report_diag(
    diag: DiagnosticMessage,
    code_map: &mut CodeMap,
    style: LabelStyle,
    severity: Severity,
) -> Result<Diagnostic<usize>, FileError> {
    let mut file_ids = HashMap::new();

    let mut labels = Vec::new();

    if let Some(label) = diag.label {
        // if the label can't be created (eg because the file its span references can't be read)
        // then skip it
        if let Ok(report_label) = label_from_source_file(&label, style, &mut file_ids, code_map) {
            labels.push(match label.text {
                Some(text) => report_label.with_message(text),
                None => report_label,
            });
        }
    }

    Ok(Diagnostic::new(severity)
        .with_message(diag.title)
        .with_labels(labels)
        .with_notes(diag.notes))
}

fn label_from_source_file<'a>(
    label: &'a DiagnosticLabel,
    style: LabelStyle,
    file_ids: &mut HashMap<&'a Path, usize>,
    code_map: &mut CodeMap
) -> Result<Label<usize>, FileError> {
    let file_path = &label.span.file;

    let file_id = match file_ids.get(file_path.as_path()) {
        Some(file_id) => *file_id,
        None => {
            let src = DefaultFilesystem.read_source(label.span.file.as_ref())?;

            let file_id = code_map.add(file_path.display().to_string(), src.into_owned());
            file_ids.insert(file_path, file_id);
            file_id
        }
    };

    let start_loc = &label.span.start;
    let end_loc = &label.span.end;

    let err_start = code_map.line_range(file_id, start_loc.line)?.start + start_loc.col;
    let err_end = code_map.line_range(file_id, end_loc.line)?.start + end_loc.col + 1;

    let label = Label::new(style, file_id, err_start..err_end);
    
    Ok(label)
}

pub fn report_err(err: &dyn DiagnosticOutput, severity: Severity) -> Result<(), FileError> {
    let mut out = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let mut code_map = CodeMap::new();

    let diag_msg = err.main();
    let main_diag = output_to_report_diag(
        diag_msg,
        &mut code_map,
        LabelStyle::Primary,
        severity,
    )?;

    codespan_reporting::term::emit(&mut out.lock(), &config, &code_map, &main_diag)?;

    let see_also_diags: Vec<_> = err
        .see_also()
        .into_iter()
        .map(|diag| {
            output_to_report_diag(diag, &mut code_map, LabelStyle::Primary, Severity::Note)
        })
        .collect::<Result<_, _>>()?;

    for diag in see_also_diags {
        writeln!(out)?;
        codespan_reporting::term::emit(&mut out.lock(), &config, &code_map, &diag)?;
    }

    out.flush()?;

    Ok(())
}
