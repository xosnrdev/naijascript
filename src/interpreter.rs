use crate::diagnostics::{AsStr, Diagnostics, Severity, Span};
use crate::syntax::parser::{
    Arena, BinOp, Block, BlockId, CmpOp, Cond, CondId, Expr, ExprId, Stmt, StmtId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    UndeclaredVariable,
    AssignmentToUndeclared,
    DivisionByZero,
    InvalidNumber,
}

impl AsStr for RuntimeErrorKind {
    fn as_str(&self) -> &'static str {
        match self {
            RuntimeErrorKind::UndeclaredVariable => "I no sabi dis variable",
            RuntimeErrorKind::AssignmentToUndeclared => {
                "You dey try give value to variable wey I no sabi"
            }
            RuntimeErrorKind::DivisionByZero => "You no fit divide by zero",
            RuntimeErrorKind::InvalidNumber => "Dis number no correct",
        }
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError<'src> {
    pub kind: RuntimeErrorKind,
    pub span: &'src Span,
}

pub struct Interpreter<'src, F: FnMut(f64)> {
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    conds: &'src Arena<Cond>,
    blocks: &'src Arena<Block>,

    env: Vec<(&'src str, f64)>,
    errors: Diagnostics,
    output: Option<F>,
}

impl<'src> Interpreter<'src, fn(f64)> {
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
    ) -> Self {
        Interpreter {
            stmts,
            exprs,
            conds,
            blocks,
            env: Vec::new(),
            errors: Diagnostics::default(),
            output: None,
        }
    }
}

impl<'src, F: FnMut(f64)> Interpreter<'src, F> {
    pub fn with_output(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
        output: F,
    ) -> Self {
        Interpreter {
            stmts,
            exprs,
            conds,
            blocks,
            env: Vec::new(),
            errors: Diagnostics::default(),
            output: Some(output),
        }
    }

    pub fn run(&mut self, root: BlockId) -> Result<(), RuntimeError<'src>> {
        let block = &self.blocks.nodes[root.0];
        for &sid in &block.stmts {
            if let Err(err) = self.exec_stmt(sid) {
                self.errors.emit(
                    err.span.clone(),
                    Severity::Error,
                    "runtime error",
                    err.kind.as_str(),
                    &[],
                );
                return Err(err);
            }
        }
        Ok(())
    }

    fn exec_stmt(&mut self, sid: StmtId) -> Result<(), RuntimeError<'src>> {
        match &self.stmts.nodes[sid.0] {
            Stmt::Assign { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.insert_or_update(var, val);
                Ok(())
            }
            Stmt::AssignExisting { var, expr, span } => {
                let val = self.eval_expr(*expr)?;
                if let Some((_, slot)) = self.env.iter_mut().find(|(name, _)| *name == *var) {
                    *slot = val;
                    Ok(())
                } else {
                    Err(RuntimeError { kind: RuntimeErrorKind::AssignmentToUndeclared, span })
                }
            }
            Stmt::Shout { expr, .. } => {
                let val = self.eval_expr(*expr)?;
                if let Some(ref mut out) = self.output {
                    out(val);
                } else {
                    println!("{val}");
                }
                Ok(())
            }
            Stmt::If { cond, then_b, else_b, .. } => {
                if self.eval_cond(*cond)? {
                    self.exec_block(*then_b)
                } else if let Some(eb) = else_b {
                    self.exec_block(*eb)
                } else {
                    Ok(())
                }
            }
            Stmt::Loop { cond, body, .. } => {
                while self.eval_cond(*cond)? {
                    self.exec_block(*body)?;
                }
                Ok(())
            }
        }
    }

    fn exec_block(&mut self, bid: BlockId) -> Result<(), RuntimeError<'src>> {
        let block = &self.blocks.nodes[bid.0];
        for &sid in &block.stmts {
            self.exec_stmt(sid)?;
        }
        Ok(())
    }

    fn eval_cond(&self, cid: CondId) -> Result<bool, RuntimeError<'src>> {
        let c = &self.conds.nodes[cid.0];
        let lhs = self.eval_expr(c.lhs)?;
        let rhs = self.eval_expr(c.rhs)?;
        let result = match c.op {
            CmpOp::Eq => lhs == rhs,
            CmpOp::Gt => lhs > rhs,
            CmpOp::Lt => lhs < rhs,
        };
        Ok(result)
    }

    fn eval_expr(&self, eid: ExprId) -> Result<f64, RuntimeError<'src>> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(n, span) => n
                .parse::<f64>()
                .map_err(|_| RuntimeError { kind: RuntimeErrorKind::InvalidNumber, span }),
            Expr::Var(v, span) => self
                .env
                .iter()
                .find(|(name, _)| *name == *v)
                .map(|(_, val)| *val)
                .ok_or(RuntimeError { kind: RuntimeErrorKind::UndeclaredVariable, span }),
            Expr::Binary { op, lhs, rhs, span } => {
                let l = self.eval_expr(*lhs)?;
                let r = self.eval_expr(*rhs)?;
                match op {
                    BinOp::Add => Ok(l + r),
                    BinOp::Minus => Ok(l - r),
                    BinOp::Times => Ok(l * r),
                    BinOp::Divide => {
                        if r == 0.0 {
                            Err(RuntimeError { kind: RuntimeErrorKind::DivisionByZero, span })
                        } else {
                            Ok(l / r)
                        }
                    }
                }
            }
        }
    }

    fn insert_or_update(&mut self, var: &'src str, val: f64) {
        if let Some((_, slot)) = self.env.iter_mut().find(|(name, _)| *name == var) {
            *slot = val;
        } else {
            self.env.push((var, val));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::parser::Parser;

    #[test]
    fn test_assignment_and_shout() {
        let src = "make x get 5\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        {
            let mut collect = |v: f64| output.push(v);
            let mut interp = Interpreter::with_output(
                &parser.stmt_arena,
                &parser.expr_arena,
                &parser.cond_arena,
                &parser.block_arena,
                &mut collect,
            );
            interp.run(root).unwrap();
        }
        assert_eq!(output, vec![5.0]);
    }

    #[test]
    fn test_reassignment() {
        let src = "make x get 2\nx get 7\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        {
            let mut collect = |v: f64| output.push(v);
            let mut interp = Interpreter::with_output(
                &parser.stmt_arena,
                &parser.expr_arena,
                &parser.cond_arena,
                &parser.block_arena,
                &mut collect,
            );
            interp.run(root).unwrap();
        }
        assert_eq!(output, vec![7.0]);
    }

    #[test]
    fn test_expression_arithmetic() {
        let src = "make x get 2 add 3 times 4\nshout(x)"; // 2 + (3*4) = 14
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        {
            let mut collect = |v: f64| output.push(v);
            let mut interp = Interpreter::with_output(
                &parser.stmt_arena,
                &parser.expr_arena,
                &parser.cond_arena,
                &parser.block_arena,
                &mut collect,
            );
            interp.run(root).unwrap();
        }
        assert_eq!(output, vec![14.0]);
    }

    #[test]
    fn test_if_statement_then() {
        let src = "make x get 1\nif to say (x na 1) start shout(42) end";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        {
            let mut collect = |v: f64| output.push(v);
            let mut interp = Interpreter::with_output(
                &parser.stmt_arena,
                &parser.expr_arena,
                &parser.cond_arena,
                &parser.block_arena,
                &mut collect,
            );
            interp.run(root).unwrap();
        }
        assert_eq!(output, vec![42.0]);
    }

    #[test]
    fn test_if_statement_else() {
        let src =
            "make x get 2\nif to say (x na 1) start shout(1) end if not so start shout(2) end";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        {
            let mut collect = |v: f64| output.push(v);
            let mut interp = Interpreter::with_output(
                &parser.stmt_arena,
                &parser.expr_arena,
                &parser.cond_arena,
                &parser.block_arena,
                &mut collect,
            );
            interp.run(root).unwrap();
        }
        assert_eq!(output, vec![2.0]);
    }

    #[test]
    fn test_loop_statement() {
        let src = "make x get 1\njasi (x small pass 4) start shout(x) x get x add 1 end";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        {
            let mut collect = |v: f64| output.push(v);
            let mut interp = Interpreter::with_output(
                &parser.stmt_arena,
                &parser.expr_arena,
                &parser.cond_arena,
                &parser.block_arena,
                &mut collect,
            );
            interp.run(root).unwrap();
        }
        assert_eq!(output, vec![1.0, 2.0, 3.0]);
    }

    #[test]
    fn test_division_by_zero_error() {
        let src = "make x get 1 divide 0\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        let mut collect = |v: f64| output.push(v);
        let mut interp = Interpreter::with_output(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
            &mut collect,
        );
        let result = interp.run(root);
        assert!(result.is_err());
        assert!(
            interp
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == RuntimeErrorKind::DivisionByZero.as_str())
        );
    }

    #[test]
    fn test_assignment_to_undeclared_variable_error() {
        let src = "x get 5";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        let mut collect = |v: f64| output.push(v);
        let mut interp = Interpreter::with_output(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
            &mut collect,
        );
        let result = interp.run(root);
        assert!(result.is_err());
        assert!(
            interp
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == RuntimeErrorKind::AssignmentToUndeclared.as_str())
        );
    }

    #[test]
    fn test_undeclared_variable_error() {
        let src = "shout(y)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut output = Vec::new();
        let mut collect = |v: f64| output.push(v);
        let mut interp = Interpreter::with_output(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
            &mut collect,
        );
        let result = interp.run(root);
        assert!(result.is_err());
        assert!(
            interp
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == RuntimeErrorKind::UndeclaredVariable.as_str())
        );
    }
}
