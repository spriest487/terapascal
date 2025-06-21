use crate::ast::util::try_parse_from_string;
use crate::ast::Stmt;
use terapascal_common::{DiagnosticOutput, Severity};

#[test]
pub fn member_bin_op_expr_is_member_stmt() {
    let result = try_parse_from_string::<Stmt>("Test", "a.B");

    match result {
        Ok(Stmt::Member(member_stmt)) => {
            assert_eq!(member_stmt.to_string(), "a.B");
            assert_eq!(member_stmt.base.to_string(), "a");
            assert_eq!(member_stmt.name.to_string(), "B");
        },

        Ok(other) => {
            panic!("expected call statement, got: {}", other)
        },

        Err(err) => {
            panic!("should parse as a valid statement, got: {}", err.err.main())
        },
    }
}
