{-# LANGUAGE TemplateHaskell #-}

module Control.Operate
  ( opdo
  ) where

import           Control.Operate.Internal
import           Control.Operate.Types
import           Data.Semigroup
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- | operate do notation
--
-- Syntax:
--
-- @
--   <lexp>     ::= <operator> ar { <opcmds> }        (opdo expression)
--   <ar>       ::= ->
--   <operator> ::= <identity>
--                | ( <identity> )                    (for section)
--   <opcmds>   ::= <opcmd_1> ... <opcmd_n> <exp> [;] (n >= 0)
--   <opcmd>    ::= <exp> ;
--                | ;                                 (empty statement)
-- @
--
-- Sematics:
--
-- @
--   [opdo| _  -> { e } |] = e
-- @
--
-- * InfixL Operator:
--
-- @
--   [opdo| op -> { stmts e } |] = op [opdo| op -> { stmts } |] e
-- @
--
-- * InfixR Operator:
--
-- @
--   [opdo| op -> { e; stmts } |] = op e [opdo| op -> { stmts } |]
-- @
--
-- Examples:
--
-- >>> [opdo| (.) -> { head; show; id } |] 1
-- '1'
--
-- >>> [opdo| . -> { head; show; id } |] 1
-- '1'
--
-- >>> [opdo| <*> -> { pure const; Just 1; Nothing } |]
-- Nothing
--
-- >>> :{
-- [opdo| const -> {
--   "str";
--   1
-- }|]
-- :}
-- "str"
--
-- >>> :{
-- "show: " ++ [opdo| (.) ->
--   \x ->
--     tail x
--   show
-- |] 10
-- :}
-- "show: 0"
--
opdo :: QuasiQuoter
opdo = QuasiQuoter
  { quoteExp  = opdoExpQ
  , quotePat  = nonsense "pattern"
  , quoteType = nonsense "type"
  , quoteDec  = nonsense "declaration"
  }
  where
    nonsense context = fail
      $  "You can't use opdo in "
      <> context
      <> " context, that doesn't even make sense."

opdoExpQ :: String -> ExpQ
opdoExpQ s = runOpdo <$> parseOperateDoExp s
