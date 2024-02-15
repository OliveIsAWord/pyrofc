main = print $ Variable $ Ident "mjau" 0

pExpr :: Parser Expr
pExpr = pChoice ExpectedExpr [
    pAndThen (pAnd (pChar '(') pExpr) (\e -> pIgnoreWith e (pChar ')'))
  ]

pIgnoreWith :: a -> Parser b -> Parser a
pIgnoreWith x p = pMap p (const x)

pMap :: Parser a -> (a -> b) -> Parser b
pMap p f str =
  case p str of
    Success x rest -> Success (f x) rest
    Failure msg -> Failure msg

pChoice :: ParseFailure -> [Parser a] -> Parser a
pChoice = undefined

pAnd :: Parser a -> Parser b -> Parser b
pAnd pa pb = pAndThen pa (const pb)

pAndThen :: Parser a -> (a -> Parser b) -> Parser b
pAndThen pa pb str =
  case pa str of
    Success x rest -> pb x rest
    Failure msg -> Failure msg

pChar :: Char -> Parser Char
pChar c = pCharIf MismatchedCharacter (== c)

pCharIf :: ParseFailure -> (Char -> Bool) -> Parser Char
pCharIf err predicate str =
  case str of
    [] -> Failure Empty
    x:xs -> case predicate x of
      True -> Success x xs
      False -> Failure err

pEof :: Parser ()
pEof str = case str of
             [] -> Success () []
             _:_ -> Failure ExpectedEof

type Parser a = String -> Parsed a

data Parsed a = Success a String
              | Failure ParseFailure
              deriving Show

data ParseFailure = Empty
                  | MismatchedCharacter
                  | ExpectedEof
                  | ExpectedExpr
                  deriving Show

data Expr = Variable Ident
          | App Expr Expr
          | Match Expr [(Ident, [Maybe Ident], Expr)] (Maybe (Maybe Ident, Expr))
          | Where Expr Decl
          deriving Show

data Decl = ValueDecl Ident [Ident] Expr
          | TyDecl Ident Ty
          deriving Show

-- A type is a list of constructor names, each with any number of type fields
newtype Ty = Ty [(Ident, [TyField])]
        deriving Show

-- A type field is a type name followed by any number of type parameters
data TyField = TyField Ident [TyField]
             deriving Show

data Ident = Ident String Int

instance Show Ident where
  show (Ident name id) = name ++ "_" ++ (show id)

-- ident_eq (Ident _ x) (Ident _ y) = x == y
