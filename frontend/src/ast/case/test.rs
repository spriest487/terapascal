use super::*;
use crate::ast::util::try_tokens_from_string;

fn try_parse_case_stmt(s: &str) -> Result<CaseStmt, String> {
    let mut src_tokens = try_tokens_from_string("test", s)?;

    CaseBlock::parse_stmt(&mut src_tokens)
        .map_err(|err| err.to_string())
}

#[test]
fn empty_case_parses() {
    let case = try_parse_case_stmt("case 1 of end").unwrap();

    assert_eq!(0, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_garbage_is_err() {
    assert!(try_parse_case_stmt("case 1 of cat dog horse end").is_err());
}

#[test]
fn case_unterminated_is_err() {
    assert!(try_parse_case_stmt("case 1 of 1: a()").is_err())
}

#[test]
fn case_with_single_branch() {
    let case = try_parse_case_stmt("case 1 of 1: a() end").unwrap();
    assert_eq!(1, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_single_branch_and_separator() {
    let case = try_parse_case_stmt("case 1 of 1: a(); end").unwrap();
    assert_eq!(1, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_separated_branches() {
    let case = try_parse_case_stmt("case 1 of 1: a(); 2: b(); 3: c() end").unwrap();
    assert_eq!(3, case.branches.len());
    assert_eq!(None, case.else_branch);
}

#[test]
fn case_with_unseparated_else() {
    let case = try_parse_case_stmt("case 1 of 1: a() else b() end").unwrap();
    assert_eq!(1, case.branches.len());
    assert!(case.else_branch.is_some());
}

#[test]
fn case_with_else_and_separator_after_final_branch() {
    let case = try_parse_case_stmt("case 1 of 1: a(); 2: b(); else c() end").unwrap();
    assert_eq!(2, case.branches.len());
    assert!(case.else_branch.is_some());
}

#[test]
fn case_with_separator_after_final_branch_and_else_stmt() {
    let case = try_parse_case_stmt("case 1 of 1: a(); 2: b(); else c(); end").unwrap();
    assert_eq!(2, case.branches.len());
    assert!(case.else_branch.is_some());
}
