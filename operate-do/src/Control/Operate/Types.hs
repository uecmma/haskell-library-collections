module Control.Operate.Types
  ( OperateDoExp (..)
  , OpdoStmt (..)
  , OperatorDirection (..)
  , OpdoOperatorInfo (..)
  , OpdoStatements (..)
  , runOpdo
  ) where

import           Language.Haskell.TH

data OpdoOperatorInfo = OpdoOperatorInfo
  { getOperatorDirection  :: OperatorDirection
  , getOperatorExpression :: Exp
  } deriving (Eq, Ord, Show)

data OpdoStatements = OpdoStatements
  { getOpCmds       :: [OpdoStmt]
  , getOpCmdLastOne :: Exp
  } deriving (Eq, Ord, Show)

data OperateDoExp = OperateDoExp
  { opdoOperator   :: OpdoOperatorInfo
  , opdoStatements :: OpdoStatements
  } deriving (Eq, Ord, Show)

data OpdoStmt
  = OpdoExpS Exp
  deriving (Show, Ord, Eq)

data OperatorDirection
  = LeftOperator
  | RightOperator
  deriving (Eq, Ord, Show, Enum)

runOpdo :: OperateDoExp -> Exp
runOpdo (OperateDoExp
  (OpdoOperatorInfo LeftOperator opExp)
  (OpdoStatements stmts expr)) = go stmts
  where
    go []               = expr
    go ~(OpdoExpS x:xs) = AppE (AppE opExp $ go' xs x) expr

    go' []               cont = cont
    go' ~(OpdoExpS x:xs) cont = go' xs $ AppE (AppE opExp cont) x
runOpdo (OperateDoExp
  (OpdoOperatorInfo RightOperator opExp)
  (OpdoStatements stmts expr)) = go stmts
  where
    go []               = expr
    go ~(OpdoExpS x:xs) = AppE (AppE opExp x) $ go xs
