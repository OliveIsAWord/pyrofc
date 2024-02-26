import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Prelude hiding (length)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      contents <- readFile filePath
      let result = mainFuncy contents
      putStrLn result
    _ -> do
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " <filepath>"
      exitFailure

mainFuncy :: String -> String
mainFuncy source = case pTokens source of
  Success x _ -> case pProgram x of
    Success x _ -> case resolveNames x of
      Left e -> error (show e)
      Right p -> compile p
    Failure f -> error (show f)
  Failure f -> error (show f)

compile :: Expr Ident -> String
compile expr = prelude ++ code ++ "\n" ++ compileTys tys
  where
    (tys, code) = go expr
    go expr = case expr of
      Var v -> ([], show v)
      App a b -> case go a of
        (atys, acode) -> case go b of
          (btys, bcode) -> (atys ++ btys, "(" ++ acode ++ " " ++ bcode ++ ")")
      Case cases ->
        (caseTys, "(\\x -> case x of { " ++ caseCodes ++ "})")
          where
            caseTys = concatMap fst casePairs
            caseCodes = concatMap snd casePairs
            casePairs = map caseMap cases
            caseMap = \(pattern, body) -> case pattern of
              Constructor name args -> case go body of
                (tys, code) -> (tys,
                  show name ++
                  concatMap ((' ':) . show) args ++
                  " -> " ++
                  code ++
                  "; ")
              Variable v -> case go body of
                (tys, code) -> (tys,
                  show v ++
                  " -> " ++
                  code ++
                  ";")
      Where expr decl ->
        case decl of
          ValueDecl name args body ->
            ( exprTys ++ bodyTys
            , "(let { " ++
              show name ++
              concatMap ((' ':) . show) args ++
              " = " ++
              bodyCode ++
              " } in " ++
              exprCode ++
              ")"
            ) where
            (bodyTys, bodyCode) = go body
          m@(TyDecl _ _ _) -> (m:exprTys, exprCode)
          where
            (exprTys, exprCode) = go expr
    compileTys tys = concatMap (('\n':) . compileTy) tys
    compileTy ty = case ty of
      TyDecl name args cons ->
        "data " ++
        show name ++
        concatMap ((' ':) . show) args ++
        (case cons of {Ty t ->
          case t of
            [] -> ""
            (t:ts) ->
              " = " ++
              showField t ++
              concatMap (\field ->
                "\n  | " ++
                showField field) ts ++
                "\n  deriving (Prelude.Eq, Prelude.Show)\n"})
        where
          showField field = case field of
            TyField name args ->
              show name ++
              concatMap (\x -> " (" ++ showField x ++ ")") args
      _ -> unreachable

prelude = "import qualified Prelude\nmain = Prelude.print Prelude.$ "

-- tt :: (Show a) => a -> a
-- tt = traceShowId

resolveNames :: Expr String -> Either NameError (Expr Ident)
resolveNames expr = eitherMap (go [] [] [] 0 expr) fst where
  go :: [Ident] -> [Ident] -> [Ident] -> Integer -> Expr String -> Either NameError (Expr Ident, Integer)
  go valEnv conEnv tyEnv i expr = case expr of
    Var name -> eitherMap (lookupEnv name valEnv) (\n -> (Var n, i))
    App a b ->
      eitherAndThen (go valEnv conEnv tyEnv i a)
      (\(a, i) -> eitherMap (go valEnv conEnv tyEnv i b) (first (App a)))
    Where body decl -> case decl of
      ValueDecl vname vargs vbody ->
        eitherAndThen (allUnique namesSeenByBinding)
        (\() -> eitherAndThen (go (head ns : valEnv) conEnv tyEnv i0 body)
        (\(body', i1) -> eitherAndThen (go (ns ++ valEnv) conEnv tyEnv i1 vbody)
        (\(r, i2) -> Right (Where body' (ValueDecl (head ns) (tail ns) r), i2))))
        where
          (ns, i0) = getFresh namesSeenByBinding i
          namesSeenByBinding = vname:vargs
      TyDecl name args ty -> case ty of
        Ty cons ->
          eitherAndThen (allUnique (name : args ++ conIds))
          (\() -> eitherAndThen bodyMeow
          (\(body', i3) -> eitherMap (tyMeow (name' : args' ++ tyEnv))
          (\ty' -> (Where body' (TyDecl name' args' ty'), i3))))
          where
            tyMeow :: [Ident] -> Either NameError (Ty Ident)
            tyMeow tyEnv = eitherMap (eitherList (map (\(TyField _ fields) -> eitherList (map visitTyField fields)) cons)) (\conFields -> Ty (map (uncurry TyField) (zip cons' conFields)))
              where
                visitTyField :: TyField String -> Either NameError (TyField Ident)
                visitTyField =
                  (\(TyField name args) -> eitherAndThen (lookupEnv name tyEnv)
                  (\name' -> eitherMap
                    (eitherList (map visitTyField args))
                    (TyField name')))
            bodyMeow = go (cons' ++ valEnv) (cons' ++ conEnv) (name' : tyEnv) i2 body
            (cons', i2) = getFresh conIds i1
            (args', i1) = getFresh args i0
            (name', i0) = (Ident name i, i + 1)
            conIds = map (\(TyField x _) -> x) cons
    Case arms -> eitherMap
      (eitherFoldr (\arm -> (\(arms, ii) -> eitherMap (meow ii arm) (\(conny, iii) -> (conny:arms, iii)))) ([], i) arms)
      (\(arms, ii) -> (Case arms, ii)) where
      meow :: Integer -> (Pattern String, Expr String) -> Either NameError ((Pattern Ident, Expr Ident), Integer)
      meow i = \(pattern, body) -> case pattern of
        Constructor con args ->
          eitherAndThen (lookupEnv con conEnv)
          (\con' -> eitherAndThen (eitherMap (allUnique (con : args))
          (\() -> getFresh args i))
          (\(args', i0) -> eitherMap (go (args' ++ valEnv) conEnv tyEnv i0 body)
          (\(body', i1) -> ((Constructor con' args', body'), i1))))
        Variable v ->
          eitherMap
            (go (Ident v i : valEnv) conEnv tyEnv (i + 1) body)
            (\(body', i0) -> ((Variable (Ident v i), body'), i0))
      
    where
      lookupEnv name env = case env of
        [] -> Left (NotFound name)
        (e:es) -> case name == getName e of
          True -> Right e
          False -> lookupEnv name es
      getFresh names i = (map (uncurry Ident) (zip names [i..]), length names + i)
  getName = (\(Ident name _) -> name)
  allUnique names = case isUnique names of
    False -> Left (Conflicting names)
    True -> Right ()
    where
      isUnique names = case names of
        [] -> True
        n:ns -> notElem n ns && isUnique ns

data NameError = NotFound String
               | Conflicting [String]
               deriving (Show, Eq)

pProgram :: Parser Token (Expr String)
pProgram = pThenIgnore pExpr pEof

pExpr :: Parser Token (Expr String)
pExpr = pDecled (pMap (pRepeat1
      (pChoice ExpectedExpr
        [ pParened (pChoice ExpectedExpr [pExpr, (pCase pExpr)])
        , pMap pTIdentEither Var
        ])) (foldl1 App))

pCase :: Parser Token (Expr String) -> Parser Token (Expr String)
pCase pExpr = pMap
  (pRepeat (pPair
    (pIgnoreThen (pElem TBar) (pChoice Idk
      [ (pMap
          (pPair pTIdentUpper (pRepeat pTIdentLower))
          (uncurry Constructor))
      , (pMap pTIdentLower Variable)
      ]))
    (pIgnoreThen (pElem TEquals) pExpr)))
  Case

pDecled :: Parser Token (Expr String) -> Parser Token (Expr String)
pDecled pExpr = pMap
  (pPair pExpr (pRepeat (pIgnoreThen (pElem TDot) (pChoice ExpectedDecl
    [ pMap
        (pPair
          (pThenIgnore (pRepeat1 pTIdentLower) (pElem TEquals))
          pExpr)
        (\(names, expr) -> ValueDecl (head names) (tail names) expr)
    , pMap
        (pPair
          (pThenIgnore (pPair pTIdentUpper (pRepeat pTIdentLower)) (pElem TEquals))
          pTyBody)
        (\(names, body) -> case names of (name, args) -> TyDecl name args body)
    ]))))
  (\(expr, decls) -> foldl Where expr decls)

pTyBody :: Parser Token (Ty String)
pTyBody = pMap (pRepeat1 pTyCon) Ty

pTyCon = pIgnoreThen (pElem TBar) pTyField

pTyField = pMap
  (pPair pTIdentUpper (pRepeat (pChoice ExpectedTyField
    [ pMap pTIdentEither (\a -> TyField a [])
    , pParened pTyField
    ])))
  (uncurry TyField)

pParened :: Parser Token a -> Parser Token a
pParened p = pThenIgnore (pIgnoreThen (pElem TParenOpen) p) (pElem TParenClose)

pTIdentEither = pChoice Idk [pTIdentUpper, pTIdentLower]

pTIdentUpper :: Parser Token String
pTIdentUpper = pElemMaybe Idk (\x -> case x of
  TIdentUpper i -> Just i
  _ -> Nothing)
  
pTIdentLower :: Parser Token String
pTIdentLower = pElemMaybe Idk (\x -> case x of
  TIdentLower i -> Just i
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
  , pMap (pRepeat1 (pElemIf Idk isAlpha)) (\ident -> case isUpper (head ident) of
    True -> TIdentUpper ident
    False -> TIdentLower ident)
  ]

pWhitespace :: Parser Char [[Char]]
pWhitespace = pRepeat (pChoice unreachable [pWhitespaceChars, pComment])

pWhitespaceChars :: Parser Char [Char]
pWhitespaceChars = pRepeat1 (pElemIf Idk isWhitespace)

pComment :: Parser Char [Char]
pComment = pIgnoreThen (pElem '#') (pRepeat (pElemIf unreachable (/= '\n')))

isWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
isAlpha c = isUpper c || isLower c
isUpper c = 'A' <= c && c <= 'Z'
isLower c = 'a' <= c && c <= 'z'

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

_pExact :: Eq s => [s] -> Parser s [s]
_pExact original = go original where
  go xs str = case xs of
    [] -> Success original str
    (y:ys) -> case str of
      [] -> Failure Empty
      (z:zs) -> case y == z of
        True -> go ys zs
        False -> Failure MismatchedCharacter

pPair :: Parser s a -> Parser s b -> Parser s (a, b)
pPair pa pb = pAndThen pa (\a -> pMap pb (\b -> (a, b)))

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

data Token = TIdentLower String
           | TIdentUpper String
           | TDot
           | TEquals
           | TParenOpen
           | TParenClose
           | TBar
           deriving (Show, Eq)

data Expr a = Var a
            | App (Expr a) (Expr a)
            | Case [(Pattern a, Expr a)]
            | Where (Expr a) (Decl a)
            deriving (Show, Eq)

data Pattern a = Constructor a [a]
               | Variable a
               deriving (Show, Eq)

data Decl a = ValueDecl a [a] (Expr a)
            | TyDecl a [a] (Ty a)
            deriving (Show, Eq)

-- A type is a list of constructors (represented as type fields)
newtype Ty a = Ty [TyField a]
             deriving (Show, Eq)

-- A type field is a type name followed by any number of type parameters
data TyField a = TyField a [TyField a]
               deriving (Show, Eq)

data Ident = Ident String Integer

instance Show Ident where
  show (Ident name id) = name ++ "_" ++ show id

instance Eq Ident where
  Ident _ a == Ident _ b = a == b

length :: [a] -> Integer
length = go 0 where
  go i xs = case xs of
    [] -> i
    (_:ys) -> go (i + 1) ys

first :: (a -> a') -> (a, b) -> (a', b)
first f = \(a, b) -> (f a, b)

eitherFoldr :: (a -> b -> Either e b) -> b -> [a] -> Either e b
eitherFoldr f acc xs = case xs of
  [] -> Right acc
  x:xs -> eitherAndThen (eitherFoldr f acc xs) (f x)

eitherList :: [Either a b] -> Either a [b]
eitherList x = go [] x where
  go as x = case x of
    [] -> Right (reverse as)
    x:xs -> eitherAndThen x (\a -> go (a:as) xs)

_eitherOr :: Either a b -> Either c b -> Either c b
_eitherOr ab cb = case ab of
  Left _ -> cb
  Right a -> Right a

eitherMap :: Either a b -> (b -> c) -> Either a c
eitherMap ab f = case ab of
  Left a -> Left a
  Right b -> Right (f b)
  
eitherAndThen :: Either a b -> (b -> Either a c) -> Either a c
eitherAndThen ab f = case ab of
  Left a -> Left a
  Right b -> f b

unreachable = error "unreachable"
