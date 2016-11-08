module Control.Operate.Internal
  ( parseOperateDoExp
  ) where

import           Control.Operate.Types
import           Data.Semigroup
import           Language.Haskell.Meta
import           Language.Haskell.TH
import           Language.Haskell.TH.Extra

parseOperateDoExp :: String -> Q OperateDoExp
parseOperateDoExp s = do
  (opInfo, restS) <- parseOperatorPrefix s
  stmts <- parseOpdoStmts restS
  return $ OperateDoExp
    { opdoOperator   = opInfo
    , opdoStatements = stmts
    }

formatOperatorExp :: String -> Q OpdoOperatorInfo
formatOperatorExp identS = do
  mayName <- lookupValueName identS
  name <- maybe (fail $ "cannot find " <> identS) return mayName
  mayFixity <- reifyFixity name
  let dir = maybe InfixL (\(Fixity _ fixityDir) -> fixityDir) mayFixity
  opDir <- case dir of
    InfixL -> return LeftOperator
    InfixR -> return RightOperator
    InfixN -> fail "InfixN operator is not supported"
  return $ OpdoOperatorInfo opDir $ VarE name

parseOperatorExp :: String -> Q OpdoOperatorInfo
parseOperatorExp ['(']    = fail "Parse error: ("
parseOperatorExp ('(':xs) = do
  let xsLeng = length xs
  let (ts, t) = splitAt (xsLeng - 1) xs
  if t == ")"
    then formatOperatorExp ts
    else fail "Cannot find ')'"
parseOperatorExp identS   = formatOperatorExp identS

parseOperatorPrefix :: String -> Q (OpdoOperatorInfo, String)
parseOperatorPrefix s = do
  let noPrefS = dropWhile isHsWhitespace s
  let (identS, postS) = break isHsWhitespace noPrefS
  opInfo <- parseOperatorExp identS
  restS <- rmArrowPrefix postS
  return (opInfo, restS)

rmArrowPrefix :: String -> Q String
rmArrowPrefix ('-':'>':xs) = return xs
rmArrowPrefix ('→':xs)     = do
  b <- isExtEnabled UnicodeSyntax
  if b
    then return xs
    else fail "Unicode arrow character is only supported with `UnicodeSyntax` Pragma"
rmArrowPrefix (x:xs)
  | isHsWhitespace x       = rmArrowPrefix xs
rmArrowPrefix []           = fail "Parse error: no statements"
rmArrowPrefix (x:_)        = fail $ "Parse error: " <> [x]

formatDoStmts :: Stmt -> Q OpdoStmt
formatDoStmts (NoBindS expr) = return $ OpdoExpS expr
formatDoStmts (LetS _)       = fail "LetS is not supported"
formatDoStmts (BindS _ _)    = fail "BindS is not supported"
formatDoStmts (ParS _)       = fail "ParS is not supported"

formatOpdoStmts :: [OpdoStmt] -> Q OpdoStatements
formatOpdoStmts [OpdoExpS expr] = return $ OpdoStatements [] expr
formatOpdoStmts (x:xs)          = do
  OpdoStatements es e <- formatOpdoStmts xs
  return $ OpdoStatements (x:es) e
formatOpdoStmts _               = fail "least an expression"

-- TODO: support this syntax (indent base parse)
--
-- @
--   [opdo| const -> 1
--                   "str"
--   |]
-- @
parseOpdoStmts :: String -> Q OpdoStatements
parseOpdoStmts stmtsStr = do
  let prefix = "do "
  stmts <- case parseExp $ prefix <> stmtsStr of
    Right (DoE stmts) -> mapM formatDoStmts stmts
    Right _           -> fail "illegal statement"
    Left msg          -> fail msg
  formatOpdoStmts stmts
