use crate::{
    ast::{BlockStatement, Expression, Identifier, IfExpression, Program, Statement},
    object::{Environment, Function, Object},
};

#[cfg(test)]
mod test;

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Environment::new(),
        }
    }

    pub fn eval_program(&mut self, program: Program) -> Object {
        // evaluation returns Null by default
        let mut result = Object::get_null();

        for stmt in program.statements {
            result = self.eval_statement(stmt);
            if let Object::ReturnValue(result) = result {
                return *result;
            }

            if let Object::Error(_) = result {
                return result;
            }
        }

        result
    }

    fn eval_statement(&mut self, statement: Statement) -> Object {
        match statement {
            Statement::Expression(expr_stmt) => self.eval_expression(expr_stmt.expression),
            Statement::Return(ret_stmt) => {
                let ret_obj = self.eval_expression(ret_stmt.ret_val);
                match ret_obj {
                    Object::Error(_) => ret_obj,
                    ret_obj => Object::ReturnValue(Box::new(ret_obj)),
                }
            }
            Statement::Let(let_stmt) => {
                let val = self.eval_expression(let_stmt.value);
                match val {
                    Object::Error(_) => val,
                    _ => {
                        self.env.set(let_stmt.name.value, val.clone());
                        val
                    }
                }
            }
            _ => Object::get_null(),
        }
    }

    fn eval_expression(&mut self, expression: Option<Expression>) -> Object {
        if let Some(expr) = expression {
            match expr {
                Expression::Integer(data) => Object::Integer(data.value),
                Expression::Boolean(data) => Object::get_bool(data.value),
                Expression::Prefix(data) => {
                    let operand = self.eval_expression(Some(*data.operand));
                    match operand {
                        Object::Error(_) => operand,
                        operand => self.eval_prefix_expression(data.operator, operand),
                    }
                }
                Expression::Infix(data) => {
                    let left = self.eval_expression(Some(*data.left));
                    match left {
                        Object::Error(_) => left,
                        _ => {
                            let right = self.eval_expression(Some(*data.right));
                            match right {
                                Object::Error(_) => right,
                                _ => self.eval_infix_expr(data.operator, left, right),
                            }
                        }
                    }
                }
                Expression::If(data) => self.eval_if_expression(data),
                Expression::Identifier(data) => self.eval_identifier(data),
                Expression::Fn(data) => Object::Fn(Function {
                    params: data.parameters,
                    body: data.body,
                    env: self.env.clone(),
                }),
                Expression::Call(data) => {
                    let func = self.eval_expression(Some(*data.function));
                    match func {
                        Object::Error(_) => func,
                        _ => {
                            let args = self.eval_expressions(data.arguments);

                            match args {
                                Err(error) => error,
                                Ok(args) => self.apply_function(func, args),
                            }
                        }
                    }
                }
                Expression::StringExp(data) => Object::String(data.value),
                Expression::Array(data) => Object::Array(
                    data.elements
                        .into_iter()
                        .map(|el| match self.eval_expression(Some(el)) {
                            Object::Error(err) => return Object::Error(err),
                            valid => valid,
                        })
                        .collect(),
                ),
                Expression::Index(data) => {
                    let operand = self.eval_expression(Some(*data.operand));
                    match operand {
                        Object::Error(_) => return operand,
                        _ => {
                            let index = self.eval_expression(Some(*data.index));

                            match index {
                                Object::Error(_) => index,
                                _ => Self::eval_index_expression(operand, index),
                            }
                        }
                    }
                }
            }
        } else {
            Object::get_null()
        }
    }

    fn eval_index_expression(operand: Object, index: Object) -> Object {
        match (index, operand) {
            (Object::Integer(idx), Object::Array(arr)) => {
                if idx < 0 {
                    return Object::get_null();
                } else {
                    arr.get(idx as usize)
                        .map(|o| o.clone())
                        .unwrap_or(Object::get_null())
                }
            }
            (index, operand) => Object::Error(format!(
                "index operator is not supported for index {index} and operand {operand}"
            )),
        }
    }
    fn apply_function(&mut self, func: Object, args: Vec<Object>) -> Object {
        match func {
            Object::Fn(mut func) => {
                // TODO: assert # of args
                // zip the args into fn's env
                for (param_name, param_val) in func.params.iter().zip(args) {
                    func.env.set(param_name.value.clone(), param_val);
                }

                let old_env = self.env.clone(); // TODO: swap the env properly
                self.env = func.env;

                let result = self.eval_block_statement(func.body);
                self.env = old_env;

                // need to unwrap in case there's an explicit return statement
                // to avoid stopping evaluation by the return
                match result {
                    Object::ReturnValue(ret_val) => *ret_val,
                    _ => result,
                }
            }
            Object::BuiltinFunction(func) => func(args),
            _ => Object::Error(format!("not a function but {}", func.type_str())),
        }
    }

    fn eval_expressions(&mut self, exprs: Vec<Expression>) -> Result<Vec<Object>, Object> {
        let mut result = vec![];
        let mut errors = vec![];
        for expression in exprs {
            let evaluated = self.eval_expression(Some(expression));
            match evaluated {
                Object::Error(msg) => errors.push(msg),
                _ => result.push(evaluated),
            };
        }
        if errors.is_empty() {
            Ok(result)
        } else {
            Err(Object::Error(format!(
                "error(s) occurred: {}",
                errors.join(", ")
            )))
        }
    }

    fn eval_prefix_expression(&self, operator: String, operand: Object) -> Object {
        match operator.as_str() {
            "!" => self.eval_bang_op(operand),
            "-" => self.eval_minus_op(operand),
            _ => Object::Error(format!(
                "unknown prefix operator {} for {}",
                operator,
                operand.type_str()
            )),
        }
    }

    fn eval_bang_op(&self, operand: Object) -> Object {
        Object::get_bool(!operand.is_truthy())
    }

    fn eval_minus_op(&self, operand: Object) -> Object {
        match operand {
            Object::Integer(val) => Object::Integer(-val),
            operand => Object::Error(format!("unknown operator: -{}", operand.type_str())),
        }
    }

    fn eval_infix_expr(&self, operator: String, left: Object, right: Object) -> Object {
        match (left, right, operator.as_str()) {
            (Object::Integer(left), Object::Integer(right), _) => {
                self.eval_integer_infix_expr(left, right, operator)
            }
            (Object::Boolean(l), Object::Boolean(r), "==") => Object::get_bool(l == r),
            (Object::Boolean(l), Object::Boolean(r), "!=") => Object::get_bool(l != r),
            (Object::String(left), Object::String(right), "+") => {
                Object::String(format!("{}{}", left, right))
            }
            (left, right, _op) => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.type_str(),
                operator,
                right.type_str()
            )),
        }
    }

    fn eval_integer_infix_expr(&self, left: i64, right: i64, operator: String) -> Object {
        match operator.as_str() {
            "+" => Object::Integer(left + right),
            "-" => Object::Integer(left - right),
            "/" => Object::Integer(left / right),
            "*" => Object::Integer(left * right),
            "<" => Object::get_bool(left < right),
            ">" => Object::get_bool(left > right),
            "==" => Object::get_bool(left == right),
            "!=" => Object::get_bool(left != right),
            op => Object::Error(format!(
                "unknown operation {} for {}",
                op,
                Object::Integer(0).type_str()
            )),
        }
    }

    fn eval_if_expression(&mut self, expr: IfExpression) -> Object {
        let cond = self.eval_expression(Some(*expr.condition));
        if cond.is_truthy() {
            self.eval_block_statement(expr.consequence)
        } else if let Some(alt) = expr.alternative {
            self.eval_block_statement(alt)
        } else {
            Object::get_null()
        }
    }

    fn eval_identifier(&self, idf: Identifier) -> Object {
        match self.env.get(&idf.value) {
            Some(val) => val.clone(),
            None => Object::Error(format!("unknown identifier '{}'", idf.value)),
        }
    }

    fn eval_block_statement(&mut self, block: BlockStatement) -> Object {
        let mut result = Object::get_null();

        for stmt in block.statements {
            result = self.eval_statement(stmt);

            // let results and errors to bubble-up
            match result {
                Object::ReturnValue(_) | Object::Error(_) => return result,
                _ => (),
            }
        }

        result
    }
}
