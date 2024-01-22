#[derive(Debug)]
enum Expr {
    Int(i32),
    Builtin(fn(i32, i32) -> i32),
    BuiltinApp(i32, fn(i32, i32) -> i32),
    App(Box<Self>, Box<Self>),
    // Where(Box<Self>,) 
}

fn eval(e: Expr) -> Expr {
    match e {
        Expr::Int(_) | Expr::Builtin(_) | Expr::BuiltinApp(..) => e,
	Expr::App(f, x) => {
	    let f = eval(*f);
	    let Expr::Int(x) = eval(*x) else {
	        panic!("Passed non-int to builtin");
	    };
	    match f {
	        Expr::Int(_) => panic!("Tried to call int"),
		Expr::App(..) => unreachable!(),
	        Expr::Builtin(fpointer) => Expr::BuiltinApp(x, fpointer),
		Expr::BuiltinApp(y, fpointer) => Expr::Int(fpointer(y, x)),
	    }
        }
    }
}

fn main() {
    let add: fn(i32, i32) -> i32 = |x, y| x + y;
    let expr = Expr::App(Box::new(Expr::App(Box::new(Expr::Builtin(add)), Box::new(Expr::Int(34)))), Box::new(Expr::Int(35)));
    println!("{:?}", eval(expr));
}
