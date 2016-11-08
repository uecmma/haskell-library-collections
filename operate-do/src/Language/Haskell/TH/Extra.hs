module Language.Haskell.TH.Extra
  ( isHsWhitespace
  ) where

import qualified Data.CharSet                  as UCS
import qualified Data.CharSet.Unicode.Category as UCS

-- | Is Haskell whitespace character
--
-- Examples:
-- >>> isHsWhitespace ' '
-- True
--
-- >>> isHsWhitespace '\r'
-- True
--
-- >>> isHsWhitespace '　'
-- True
--
-- >>> isHsWhitespace 'w'
-- False
--
isHsWhitespace :: Char -> Bool
isHsWhitespace c = UCS.member c $
  foldl1 UCS.union
  [ UCS.space
  , UCS.singleton '\t'
  , UCS.singleton '\v'
  , UCS.singleton '\r'
  , UCS.singleton '\n'
  , UCS.singleton '\f'
  ]
