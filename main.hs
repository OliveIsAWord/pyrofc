data Expr = Variable String
          | App Expr Expr
          | Match Expr [(String, [Maybe String], Expr)] (Maybe (Maybe String, Expr))
          | Where Expr Decl
          deriving Show

data Decl = Value [String] Expr
          | Ty [(String, [String])]
          deriving Show

data Value = Constructor String [Value]

main = putStrLn $ show $ Variable "mjau"