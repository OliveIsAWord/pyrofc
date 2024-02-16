import Prelude hiding (uncurry)

main = print main_funcy

main_funcy = case pTokens "one two (three four . Option a = | Some a | None . four = three five) five" of
  Success x _ -> pProgram x
  Failure f -> Failure f

pProgram :: Parser Token Expr
pProgram = pThenIgnore pExpr pEof

pExpr :: Parser Token Expr
pExpr = pDecled (pMap (pRepeat1
      (pChoice ExpectedExpr
        [ pThenIgnore (pIgnoreThen (pElem TParenOpen) pExpr) (pElem TParenClose)
        , pMap pTIdent Variable
        ])) (foldl1 App))

pDecled pExpr = pMap
  (pPair pExpr (pRepeat
    (pMap
      (pPair
        (pMap
          (pThenIgnore (pIgnoreThen (pElem TDot) (pRepeat1 pTIdent)) (pElem TEquals))
          (\xs -> case xs of [] -> unreachable; (y:ys) -> MkPair y ys))
        (pChoice ExpectedDecl
          [ (pMap pExpr (\expr decls -> case decls of
              MkPair name args -> ValueDecl name args expr))
          , (pMap pTyBody (\ty decls -> case decls of
              MkPair name args -> TyDecl name args ty))
          ]))
      (\p -> case p of MkPair idents f -> f idents))))
  (\p -> case p of MkPair expr decls -> foldl Where expr decls)

pTyBody :: Parser Token Ty
pTyBody = pMap (pRepeat1 pTyCon) Ty

pTyCon = pIgnoreThen (pElem TBar) pTyField

pTyField = pMap
  (pPair pTIdent (pRepeat (pChoice ExpectedTyField
    [ pTyField
    , pParened pTyField
    ])))
  (uncurry TyField)

pParened :: Parser Token a -> Parser Token a
pParened p = pThenIgnore (pIgnoreThen (pElem TParenOpen) p) (pElem TParenClose)

pTIdent :: Parser Token Ident
pTIdent = pElemMaybe Idk (\x -> case x of
  TIdent i -> Just i
  _ -> Nothing)

pTokens :: Parser Char [Token]
pTokens = pThenIgnore
  (pRepeat (pIgnoreThen pWhitespace pToken))
  (pIgnoreThen pWhitespace pEof)

pToken :: Parser Char Token
pToken = pChoice ExpectedToken
  [ pTo TDot (pElem '.')
  , pTo TEquals (pElem '=')
  , pTo TParenOpen (pElem '(')
  , pTo TParenClose (pElem ')')
  , pTo TBar (pElem '|')
  , pMap (pRepeat1 (pElemIf Idk isAlpha)) (\s -> TIdent (Ident s 0))
  ]

pWhitespace :: Parser Char [Char]
pWhitespace = pRepeat (pElemIf unreachable isWhitespace)

isWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
isAlpha c = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

pRepeat1 :: Parser s a -> Parser s [a]
pRepeat1 p str = case p str of
  Success x xs -> case pRepeat p xs of
    Success y ys -> Success (x:y) ys
    Failure _ -> unreachable
  Failure f -> Failure f

pRepeat :: Parser s a -> Parser s [a]
pRepeat p = go [] where
  go acc str = case p str of
    Success x xs -> go (x:acc) xs
    Failure _ -> Success (reverse acc) str

pTo :: a -> Parser s b -> Parser s a
pTo x p = pMap p (const x)

pThenIgnore :: Parser s a -> Parser s b -> Parser s a
pThenIgnore pa pb = pAndThen pa (\a -> pTo a pb)

pIgnoreThen :: Parser s a -> Parser s b -> Parser s b
pIgnoreThen pa pb = pAndThen pa (const pb)

pMap :: Parser s a -> (a -> b) -> Parser s b
pMap p f str =
  case p str of
    Success x rest -> Success (f x) rest
    Failure msg -> Failure msg

pChoice :: ParseFailure -> [Parser s a] -> Parser s a
pChoice fail ps = go ps where
  go ps str = case ps of
    [] -> Failure fail
    (p:ps) -> case p str of
      Success x xs -> Success x xs
      Failure _ -> go ps str

pExact :: Eq s => [s] -> Parser s [s]
pExact original = go original where
  go xs str = case xs of
    [] -> Success original str
    (y:ys) -> case str of
      [] -> undefined

pPair :: Parser s a -> Parser s b -> Parser s (Pair a b)
pPair pa pb = pAndThen pa (\a -> pMap pb (\b -> MkPair a b))

pAndThen :: Parser s a -> (a -> Parser s b) -> Parser s b
pAndThen pa pb str =
  case pa str of
    Success x rest -> pb x rest
    Failure msg -> Failure msg

pElem :: Eq s => s -> Parser s s
pElem c = pElemIf MismatchedCharacter (== c)

pElemMaybe :: ParseFailure -> (a -> Maybe b) -> Parser a b
pElemMaybe err predicate str =
  case str of
    [] -> Failure Empty
    x:xs -> case predicate x of
      Just y -> Success y xs
      Nothing -> Failure err

pElemIf :: ParseFailure -> (a -> Bool) -> Parser a a
pElemIf err predicate str =
  case str of
    [] -> Failure Empty
    x:xs -> case predicate x of
      True -> Success x xs
      False -> Failure err

pEof :: Parser a ()
pEof xs = case xs of
             [] -> Success () []
             _:_ -> Failure ExpectedEof

type Parser s a = [s] -> Parsed s a

data Parsed s a = Success a [s]
              | Failure ParseFailure
              deriving (Show, Eq)

data ParseFailure = Empty
                  | MismatchedCharacter
                  | ExpectedEof
                  | ExpectedExpr
                  | ExpectedDecl
                  | ExpectedTyField
                  | ExpectedToken
                  | Idk
                  deriving (Show, Eq)

data Token = TIdent Ident
           | TDot
           | TEquals
           | TParenOpen
           | TParenClose
           | TBar
           deriving (Show, Eq)

data Expr = Variable Ident
          | App Expr Expr
          | Match Expr [(Ident, [Maybe Ident], Expr)] (Maybe (Maybe Ident, Expr))
          | Where Expr Decl
          deriving (Show, Eq)

data Decl = ValueDecl Ident [Ident] Expr
          | TyDecl Ident [Ident] Ty
          deriving (Show, Eq)

-- A type is a list of constructors (represented as type fields)
newtype Ty = Ty [TyField]
        deriving (Show, Eq)

-- A type field is a type name followed by any number of type parameters
data TyField = TyField Ident [TyField]
             deriving (Show, Eq)

data Ident = Ident String Int

instance Show Ident where
  show (Ident name id) = name ++ "_" ++ (show id)

instance Eq Ident where
  Ident _ a == Ident _ b = a == b

uncurry f ab = case ab of
  MkPair a b -> f a b

data Pair a b = MkPair a b

unreachable = error "unreachable"
