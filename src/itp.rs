use std::fmt;

use crate::syntax::parse::{
    Arena, BinOp, Block, BlockId, CmpOp, Cond, CondId, Expr, ExprId, Stmt, StmtId,
};

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: &'static str,
    pub context: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.message, self.context)
    }
}

pub struct Interpreter<'src> {
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    conds: &'src Arena<Cond>,
    blocks: &'src Arena<Block>,

    env: Vec<(&'src str, f64)>,
    pub errors: Vec<RuntimeError>,
}

impl<'src> Interpreter<'src> {
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
    ) -> Self {
        Interpreter { stmts, exprs, conds, blocks, env: Vec::new(), errors: Vec::new() }
    }

    pub fn run(&mut self, root: BlockId) -> Result<(), RuntimeError> {
        let block = &self.blocks.nodes[root.0];
        for &sid in &block.stmts {
            if let Err(err) = self.exec_stmt(sid) {
                self.errors.push(err.clone());
                return Err(err);
            }
        }
        Ok(())
    }

    fn exec_stmt(&mut self, sid: StmtId) -> Result<(), RuntimeError> {
        match &self.stmts.nodes[sid.0] {
            Stmt::Assign { var, expr } => {
                let val = self.eval_expr(*expr)?;
                self.insert_or_update(var, val);
                Ok(())
            }
            Stmt::AssignExisting { var, expr } => {
                let val = self.eval_expr(*expr)?;
                if let Some((_, slot)) = self.env.iter_mut().find(|(name, _)| *name == *var) {
                    *slot = val;
                    Ok(())
                } else {
                    Err(RuntimeError {
                        message: "assignment to undeclared variable at runtime",
                        context: (*var).to_string(),
                    })
                }
            }
            Stmt::Shout { expr } => {
                let val = self.eval_expr(*expr)?;
                println!("{val}");
                Ok(())
            }
            Stmt::If { cond, then_b, else_b } => {
                if self.eval_cond(*cond)? {
                    self.exec_block(*then_b)
                } else if let Some(eb) = else_b {
                    self.exec_block(*eb)
                } else {
                    Ok(())
                }
            }
            Stmt::Loop { cond, body } => {
                while self.eval_cond(*cond)? {
                    self.exec_block(*body)?;
                }
                Ok(())
            }
        }
    }

    fn exec_block(&mut self, bid: BlockId) -> Result<(), RuntimeError> {
        let block = &self.blocks.nodes[bid.0];
        for &sid in &block.stmts {
            self.exec_stmt(sid)?;
        }
        Ok(())
    }

    fn eval_cond(&self, cid: CondId) -> Result<bool, RuntimeError> {
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

    fn eval_expr(&self, eid: ExprId) -> Result<f64, RuntimeError> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(n) => n
                .parse::<f64>()
                .map_err(|_| RuntimeError { message: "invalid number", context: n.to_string() }),
            Expr::Var(v) => {
                self.env.iter().find(|(name, _)| *name == *v).map(|(_, val)| *val).ok_or(
                    RuntimeError {
                        message: "undeclared variable at runtime",
                        context: (*v).to_string(),
                    },
                )
            }
            Expr::Binary { op, lhs, rhs } => {
                let l = self.eval_expr(*lhs)?;
                let r = self.eval_expr(*rhs)?;
                match op {
                    BinOp::Add => Ok(l + r),
                    BinOp::Minus => Ok(l - r),
                    BinOp::Times => Ok(l * r),
                    BinOp::Divide => {
                        if r == 0.0 {
                            Err(RuntimeError {
                                message: "division by zero",
                                context: format!("{l} divide {r}"),
                            })
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
