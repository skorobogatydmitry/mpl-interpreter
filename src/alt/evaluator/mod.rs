use std::cell::Cell;

use crate::alt::{
    ast::{expression::If, Expression, Program, Statement},
    object::{Environment, Function, Object},
};

use super::{
    object::{self, NULL},
    token::Token,
};

#[cfg(test)]
mod test;

pub struct Evaluator {
    env: Cell<Environment>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Cell::new(Environment::new()),
        }
    }

    pub fn eval_program(&mut self, program: Program) -> Result<Object, String> {
        self.eval_statements(program.statements).map(|obj| {
            if let Object::ReturnValue(inner) = obj {
                *inner
            } else {
                obj
            }
        })
    }

    fn eval_statements(&mut self, statements: Vec<Statement>) -> Result<Object, String> {
        // evaluation returns Null by default
        let mut result = object::NULL;

        for stmt in statements {
            result = self.eval_statement(stmt)?;
            if let Object::ReturnValue(_) = result {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_statement(&mut self, statement: Statement) -> Result<Object, String> {
        match statement {
            Statement::Expression(expression) => self.eval_expression(expression),
            Statement::Return(ret_stmt) => {
                let ret_obj = self.eval_expression(ret_stmt.ret_expr)?;
                Ok(Object::ReturnValue(Box::new(ret_obj)))
            }
            Statement::Let(let_stmt) => {
                let val = self.eval_expression(let_stmt.value)?;
                self.env.get_mut().set(let_stmt.name, val.clone())?;
                Ok(val)
            }
            // TODO: swap env
            Statement::Block(block) => self.eval_statements(block.statements),
        }
    }

    fn eval_expression(&mut self, expression: Expression) -> Result<Object, String> {
        match expression {
            Expression::Integer(val) => Ok(Object::Integer(val)),
            Expression::Boolean(val) => Ok(Object::get_bool(val)),
            Expression::Prefix(val) => {
                let operand = self.eval_expression(*val.operand)?;
                Self::eval_prefix_expression(val.operator, operand)
            }
            Expression::Infix(data) => {
                let left = self.eval_expression(*data.left)?;
                let right = self.eval_expression(*data.right)?;
                Self::eval_infix_expr(data.operator, left, right)
            }
            Expression::If(data) => self.eval_if_expression(data),
            Expression::Identifier(data) => self
                .env
                .get_mut()
                .get(&data)
                .ok_or(format!("unknown identifier '{}'", data))
                .map(|obj| obj.clone()), // TODO: don't clone object to allow inplace modifications: `let x = [1,2,3]; rest(x); first(x) == 2; // true`
            Expression::Fn(data) => Ok(Object::Fn(Function {
                params: data.parameters,
                body: data.body,
                env: self.env.get_mut().clone(), // capture environment where the fn is defined
            })),
            Expression::Call(data) => {
                let func = self.eval_expression(*data.function)?;
                let args = self.eval_expressions(data.arguments)?;
                self.apply_function(func, args)
            }
            Expression::String(data) => Ok(Object::String(data)),
            Expression::Array(data) => Ok(Object::Array(
                data.into_iter()
                    .map(|el| Ok(self.eval_expression(el)?))
                    .collect::<Result<Vec<Object>, String>>()?,
            )),
            Expression::Index(data) => {
                let operand = self.eval_expression(*data.operand)?;
                let index = self.eval_expression(*data.index)?;
                match (operand, index) {
                    (Object::Array(arr), Object::Integer(idx)) => {
                        if idx < 0 {
                            Ok(NULL)
                        } else {
                            // TODO: return the original object to allow inplace modifications: `let arr = [1,2,3]; let x = arr[0]; x += 1; first(arr) == 2;`
                            Ok(arr.get(idx as usize).map(|o| o.clone()).unwrap_or(NULL))
                        }
                    }
                    (operand, index) => Err(format!("cannot index {operand} with {index}")),
                }
            }
            Expression::Hash(_) => todo!(),
            Expression::Empty => Ok(object::NULL),
        }
    }

    fn apply_function(&mut self, func: Object, args: Vec<Object>) -> Result<Object, String> {
        match func {
            Object::Fn(mut func) => {
                if func.params.len() != args.len() {
                    return Err(format!(
                        "expected {} args, found {} for fn call",
                        args.len(),
                        func.params.len()
                    ));
                }
                // zip the args into fn's env
                for (param_name, param_val) in func.params.iter().zip(args) {
                    func.env.set(param_name.clone(), param_val)?;
                }

                let orig_env = self.env.replace(func.env);
                let result = self.eval_statements(func.body.statements);
                self.env.replace(orig_env);

                result
            }
            Object::BuiltinFn(func) => (func.f)(args),
            _ => Err(format!("not a function but {}", func.type_str())),
        }
    }

    fn eval_expressions(&mut self, exprs: Vec<Expression>) -> Result<Vec<Object>, String> {
        let mut result = vec![];
        let mut errors = vec![];
        for expression in exprs {
            match self.eval_expression(expression) {
                Ok(res) => result.push(res),
                Err(error) => errors.push(error),
            }
        }
        if errors.is_empty() {
            Ok(result)
        } else {
            Err(format!(
                "error evaluating fn call args: {}",
                errors.join(", ")
            ))
        }
    }

    fn eval_prefix_expression(operator: Token, operand: Object) -> Result<Object, String> {
        match operator {
            Token::Bang => Ok(Self::eval_bang_op(operand)),
            Token::Minus => Self::eval_minus_op(operand),
            _ => Err(format!(
                "unsupported prefix operator {} for {}",
                operator,
                operand.type_str()
            )),
        }
    }

    fn eval_bang_op(operand: Object) -> Object {
        Object::get_bool(!operand.is_truthy())
    }

    fn eval_minus_op(operand: Object) -> Result<Object, String> {
        match operand {
            Object::Integer(val) => Ok(Object::Integer(-val)),
            operand => Err(format!(
                "unsupported operator: - for {}",
                operand.type_str()
            )),
        }
    }

    fn eval_infix_expr(operator: Token, left: Object, right: Object) -> Result<Object, String> {
        Ok(match (left, right, operator) {
            (Object::Integer(left), Object::Integer(right), operator) => {
                Self::eval_integer_infix_expr(left, right, operator)?
            }
            (Object::Boolean(l), Object::Boolean(r), Token::Eq) => Object::get_bool(l == r),
            (Object::Boolean(l), Object::Boolean(r), Token::NotEq) => Object::get_bool(l != r),
            (Object::String(l), Object::String(r), Token::Plus) => {
                Object::String(format!("{}{}", l, r))
            }
            (l, r, operator) => {
                return Err(format!(
                    "unsupported operation: {} {} {}",
                    l.type_str(),
                    operator,
                    r.type_str()
                ))
            }
        })
    }

    fn eval_integer_infix_expr(left: i64, right: i64, operator: Token) -> Result<Object, String> {
        Ok(match operator {
            Token::Plus => Object::Integer(left + right),
            Token::Minus => Object::Integer(left - right),
            Token::Slash => Object::Integer(left / right),
            Token::Asterisk => Object::Integer(left * right),
            Token::Lt => Object::get_bool(left < right),
            Token::Gt => Object::get_bool(left > right),
            Token::Eq => Object::get_bool(left == right),
            Token::NotEq => Object::get_bool(left != right),
            op => {
                return Err(format!(
                    "unsupported operation {} for {}",
                    op,
                    object::INT_ZERO.type_str()
                ))
            }
        })
    }

    fn eval_if_expression(&mut self, expr: If) -> Result<Object, String> {
        let cond = self.eval_expression(*expr.condition)?;
        if cond.is_truthy() {
            self.eval_statements(expr.consequence.statements)
        } else if let Some(alt) = expr.alternative {
            self.eval_statements(alt.statements)
        } else {
            Ok(object::NULL)
        }
    }
}
