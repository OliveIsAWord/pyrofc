import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

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

compile :: Expr Ident -> String
compile expr = prelude ++ code ++ "\n" ++ compileTys tys
  where
    (tys, code) = go expr
    go expr = case expr of
      Var v -> ([], show v)
      App a b -> case go a of
        (atys, acode) -> case go b of
          (btys, bcode) -> (atys ++ btys, "(" ++ acode ++ " " ++ bcode ++ ")")
      Case cases defaultCase ->
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

resolveNames :: Expr String -> Either NameError (Expr Ident)
resolveNames = go [] [] [] where
  go valEnv conEnv tyEnv expr = case expr of
    Var name -> case lookupEnv name valEnv of
      Just i -> Right (Var i)
      Nothing -> Left (NotFound name)
    App a b ->
      eitherAndThen (go valEnv conEnv tyEnv a)
      (\a -> eitherMap (go valEnv conEnv tyEnv b) (App a))
    Where body decl -> case decl of
      ValueDecl vname vargs vbody ->
        eitherAndThen (allUnique namesSeenByBinding)
        (\() -> eitherAndThen (go (pushEnv vname valEnv) conEnv tyEnv body)
        (\body' -> eitherAndThen (go (ns ++ valEnv) conEnv tyEnv vbody)
        (\r -> Right (Where body' (ValueDecl (head ns) (tail ns) r)))))
        where
          ns = getFresh namesSeenByBinding valEnv
          namesSeenByBinding = vname:vargs
      TyDecl name args ty -> case ty of
        Ty cons ->
          eitherAndThen bodyMeow
          (\body' -> eitherMap tyMeow
          (\ty' -> Where body' (TyDecl name' ccons ty')))
          where
            tyMeow = eitherMap (eitherList (map goTy cons)) Ty
            goTy :: TyField String -> Either NameError (TyField Ident)
            goTy = error "type name resolution"
            bodyMeow = go (vcons ++ valEnv) (ccons ++ conEnv) (name':tyEnv) body
            vcons = getFresh conIds valEnv
            ccons = getFresh conIds conEnv
            name' = head (getFresh [name] tyEnv)
            conIds = map (\(TyField n _) -> n) cons
    Case arms fallback -> meow where
      meow = case head arms of
        (pattern, body) -> case pattern of
          Constructor con args ->
            eitherAndThen (case lookupEnv con conEnv of
              Just _ -> Right ()
              Nothing -> Left (NotFound con))
            (\() -> eitherAndThen (go (appendEnv args valEnv) conEnv tyEnv body) (error "rest of case name resolution"))
      
    where
      lookupEnv name env = case env of
        [] -> Nothing
        (e:es) -> case name == getName e of
          True -> Just e
          False -> lookupEnv name es
      pushEnv name env = appendEnv [name] env
      appendEnv names env = getFresh names env ++ env
      getFresh names env = map (uncurry Ident) (zip names [length env..])
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
        , pMap pTIdent Var
        ])) (foldl1 App))

pCase :: Parser Token (Expr String) -> Parser Token (Expr String)
pCase pExpr = pMap
  (pRepeat (pPair
    (pIgnoreThen (pElem TBar) (pMap (pRepeat1 pTIdent) (\xs -> case xs of
      [] -> unreachable
      (x:xs) -> Constructor x xs)))
    (pIgnoreThen (pElem TEquals) pExpr)))
  (\con -> Case con Nothing)

pDecled :: Parser Token (Expr String) -> Parser Token (Expr String)
pDecled pExpr = pMap
  (pPair pExpr (pRepeat
    (pMap
      (pPair
        (pMap
          (pThenIgnore (pIgnoreThen (pElem TDot) (pRepeat1 pTIdent)) (pElem TEquals))
          (\xs -> case xs of [] -> unreachable; (y:ys) -> (y, ys)))
        (pChoice ExpectedDecl
          [ (pMap pExpr (\expr decls -> case decls of
              (name, args) -> ValueDecl name args expr))
          , (pMap pTyBody (\ty decls -> case decls of
              (name, args) -> TyDecl name args ty))
          ]))
      (\(idents, f) -> f idents))))
  (\(expr, decls) -> foldl Where expr decls)

pTyBody :: Parser Token (Ty String)
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

pTIdent :: Parser Token String
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
  , pMap (pRepeat1 (pElemIf Idk isAlpha)) TIdent
  ]

pWhitespace :: Parser Char [[Char]]
pWhitespace = pRepeat (pChoice unreachable [pWhitespaceChars, pComment])

pWhitespaceChars :: Parser Char [Char]
pWhitespaceChars = pRepeat1 (pElemIf Idk isWhitespace)

pComment :: Parser Char [Char]
pComment = pIgnoreThen (pElem '#') (pRepeat (pElemIf unreachable (/= '\n')))

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

data Token = TIdent String
           | TDot
           | TEquals
           | TParenOpen
           | TParenClose
           | TBar
           deriving (Show, Eq)

data Expr a = Var a
            | App (Expr a) (Expr a)
            | Case
              [(Pattern a, Expr a)]
              (Maybe (Maybe a, Expr a))
            | Where (Expr a) (Decl a)
            deriving (Show, Eq)

data Pattern a = Constructor a [a]
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

data Ident = Ident String Int

instance Show Ident where
  show (Ident name id) = name ++ "_" ++ show id

instance Eq Ident where
  Ident _ a == Ident _ b = a == b

eitherList :: [Either a b] -> Either a [b]
eitherList x = eitherMap (go [] x) reverse where
  go as x = case x of
    [] -> Right []
    x:xs -> eitherAndThen x (\a -> go (a:as) xs) 

eitherMap :: Either a b -> (b -> c) -> Either a c
eitherMap ab f = case ab of
  Left a -> Left a
  Right b -> Right (f b)
  
eitherAndThen :: Either a b -> (b -> Either a c) -> Either a c
eitherAndThen ab f = case ab of
  Left a -> Left a
  Right b -> f b

unreachable = error "unreachable"
