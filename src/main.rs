#![feature(generators, generator_trait, nll)]

use combine::parser::char::*;
use combine::*;

use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
enum Value {
    Nil,
    Integer(i64),
    Symbol(String),
    Cons(Box<Value>, Box<Value>),
    Lambda {
        params: Box<Value>,
        body: Box<Value>,
        // TODO: add environment
    },
    NativeFun {
        body: Rc<dyn Fn(Value) -> Value>,
        name: String,
    },
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Value::*;
        match *self {
            Nil => f.debug_struct("Value::Nil").finish(),
            Integer(ref x) => f.debug_tuple("Value::Integer").field(x).finish(),
            Symbol(ref s) => f.debug_tuple("Value::Symbol").field(s).finish(),
            // TODO: pretty-print like (x y z) and (x . y)
            Cons(ref car, ref cdr) => f.debug_tuple("Value::Cons").field(car).field(cdr).finish(),
            Lambda {
                ref params,
                ref body,
            } => f
                .debug_struct("Value::Lambda")
                .field("params", params)
                .field("body", body)
                .finish(),
            NativeFun { ref name, .. } => f.debug_tuple("Value::NativeFun").field(name).finish(),
        }
    }
}

fn print_cons(
    car: &Value,
    cdr: &Value,
    f: &mut std::fmt::Formatter,
    start: bool,
) -> std::fmt::Result {
    use self::Value::*;

    if start {
        write!(f, "({}", car)?;
    } else {
        write!(f, " {}", car)?;
    }

    match *cdr {
        Nil => write!(f, ")"),
        Cons(ref cadr, ref cddr) => print_cons(cadr, cddr, f, false),
        ref cdr => write!(f, " . {})", cdr),
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Value::*;
        match *self {
            Nil => write!(f, "()"),
            Integer(ref x) => write!(f, "{}", x),
            Symbol(ref s) => write!(f, "{}", s),
            Cons(ref car, ref cdr) => print_cons(car, cdr, f, true),
            Lambda {
                ref params,
                ref body,
            } => write!(f, "(lambda {} {})", params, body),
            NativeFun { ref name, .. } => write!(f, "<function {}>", name),
        }
    }
}

fn symbol<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1(letter())
}

fn integer<I>() -> impl Parser<Input = I, Output = i64>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    from_str(many1::<String, _>(digit()))
}

fn list<I>() -> impl Parser<Input = I, Output = Value>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    char('(')
        .with(spaces())
        .with(sep_by(expr(), space().skip(spaces())))
        .skip(spaces())
        .skip(char(')'))
        .map(|mut values: Vec<_>| {
            let mut ret = Value::Nil;
            while let Some(last) = values.pop() {
                ret = Value::Cons(Box::new(last), Box::new(ret));
            }
            ret
        })
}

fn expr_<I>() -> impl Parser<Input = I, Output = Value>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice((
        list(),
        integer().map(Value::Integer),
        symbol().map(Value::Symbol),
    ))
}

parser! {
    fn expr[I]()(I) -> Value
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ]
    {
        expr_()
    }
}

fn program<I>() -> impl Parser<Input = I, Output = Value>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    spaces().with(many(list().skip(spaces())).map(|mut values: Vec<_>| {
        let mut ret = Value::Nil;
        while let Some(last) = values.pop() {
            ret = Value::Cons(Box::new(last), Box::new(ret));
        }
        ret
    }))
}

#[derive(Debug)]
struct Env {
    stacks: Vec<HashMap<String, Value>>,
}

impl Env {
    fn new() -> Self {
        use self::Value::*;

        let mut global = HashMap::new();
        global.insert(
            "plus".into(),
            NativeFun {
                body: Rc::new(|mut args| {
                    let mut result = 0;
                    loop {
                        match args {
                            Nil => return Integer(result),
                            Cons(car, cdr) => match *car {
                                Integer(x) => {
                                    result += x;
                                    args = *cdr;
                                }
                                car => panic!("type error: {:#?}", Cons(Box::new(car), cdr)),
                            },
                            args => panic!("type error: {:#?}", args),
                        }
                    }
                }),
                name: "plus".into(),
            },
        );
        global.insert(
            "println".into(),
            NativeFun {
                body: Rc::new(|mut args| {
                    let mut first = true;
                    loop {
                        match args {
                            Nil => {
                                println!("");
                                return Nil;
                            }
                            Cons(car, cdr) => {
                                if first {
                                    print!("{}", car);
                                } else {
                                    print!(" {}", car);
                                }
                                match *cdr {
                                    Nil => {
                                        println!("");
                                        break;
                                    }
                                    cdr => args = cdr,
                                }
                            }
                            _ => unreachable!("something wrong"),
                        }
                        first = false;
                    }
                    Nil
                }),
                name: "println".into(),
            },
        );

        Env {
            stacks: vec![global],
        }
    }

    fn lookup(&self, name: &str) -> Option<&Value> {
        for stack in self.stacks.iter().rev() {
            if let Some(value) = stack.get(name) {
                return Some(value);
            }
        }
        None
    }
}

fn interp_args(ast: Value, env: &mut Env) -> Value {
    use self::Value::*;
    match ast {
        Nil => Nil,
        Cons(car, cdr) => Cons(
            Box::new(interp(*car, env)),
            Box::new(interp_args(*cdr, env)),
        ),
        _ => panic!("something wrong"),
    }
}

fn interp(ast: Value, env: &mut Env) -> Value {
    use self::Value::*;

    match ast {
        Nil | Integer(_) | Lambda { .. } | NativeFun { .. } => ast,
        Symbol(name) => env
            .lookup(&name)
            .expect(&format!("{} not found", name))
            .clone(),
        Cons(car, cdr) => {
            match *car {
                // special forms
                Symbol(ref s) if s == "define" => unimplemented!(),
                Symbol(ref s) if s == "lambda" => {
                    // (lambda (x y z...) exprs...)
                    match *cdr {
                        Cons(params, body) => Lambda { params, body },
                        _ => panic!(),
                    }
                }
                Symbol(ref s) if s == "cond" => unimplemented!(),
                Symbol(ref s) if s == "let" => unimplemented!(),
                Symbol(ref s) if s == "begin" => unimplemented!(),
                Symbol(ref s) if s == "quote" => unimplemented!(),
                // function call
                car => {
                    let car = interp(car, env);
                    let mut args = interp_args(*cdr, env);
                    match car {
                        Lambda { mut params, body } => {
                            let mut new_stack = HashMap::new();
                            loop {
                                match (*params, args) {
                                    (Cons(car_params, cdr_params), Cons(arg, cdr_args)) => {
                                        if let Symbol(name) = *car_params {
                                            new_stack.insert(name, *arg);
                                            params = cdr_params;
                                            args = *cdr_args;
                                        } else {
                                            panic!("not a variable")
                                        }
                                    }
                                    (Nil, Nil) => break,
                                    _ => panic!("arguments mismatch"),
                                }
                            }
                            env.stacks.push(new_stack);
                            let ret = interp_list(*body, env);
                            env.stacks.pop();
                            ret
                        }
                        NativeFun { body, .. } => {
                            let new_stack = HashMap::new();
                            env.stacks.push(new_stack);
                            let ret = body(args);
                            env.stacks.pop();
                            ret
                        }
                        car => panic!("not a function: {}", car),
                    }
                }
            }
        }
    }
}

fn interp_list(ast: Value, env: &mut Env) -> Value {
    use self::Value::*;
    match ast {
        Nil => Nil,
        Cons(car, cdr) => match *cdr {
            Nil => interp(*car, env),
            cdr => {
                interp(*car, env);
                interp_list(cdr, env)
            }
        },
        _ => panic!("type error"),
    }
}

fn interp_top(mut ast: Value) {
    use self::Value::*;

    let mut env = Env::new();
    loop {
        match ast {
            Nil => return,
            Cons(car, cdr) => {
                interp(*car, &mut env);
                ast = *cdr;
            }
            _ => {}
        }
    }
}

fn main() {
    let code = r#"
(println ((lambda (x y) (plus x y)) 5 4))
(println 1 2 3 4)
"#;
    match program().easy_parse(code) {
        Ok((ast, remaining)) => {
            println!("remaining: {}\nast: {}", remaining, ast);
            interp_top(ast);
        }
        Err(e) => println!(
            "{}\nat: {}",
            e,
            &code[e.position.translate_position(code)..]
        ),
    }
}
