-- | A monad transformer for the 'Quote' type class from template-haskell.
--
-- The ["Overloaded Quotations" proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0246-overloaded-bracket.rst)
-- has been implemented in GHC 9.0, and generalizes the type of quotation brackets from
--
-- @[| \\x -> x + 1 |] ::            'Language.Haskell.TH.Q' 'Language.Haskell.TH.Exp'@
--
-- to
--
-- @[| \\x -> x + 1 |] :: 'Quote' m => m 'Language.Haskell.TH.Exp'@
--
-- where the 'Quote' type class only has a single function, 'newName', in order to generate
-- fresh 'Language.Haskell.TH.Name's.
--
-- By default, template-haskell provides 'Quote' instances for 'Language.Haskell.TH.Q' and 'IO'.
-- This library provides pure versions, 'PureQ' and its transformer variant 'QuoteT', in order to
-- e.g. extract an 'Language.Haskell.TH.Exp' from the bracket above in a pure context.
-- See 'runPureQ' and 'runQuoteT' for concrete examples.
module Control.Monad.Quote
  ( -- * The @Quote@ class
    Quote (..),

    -- * The @PureQ@ monad
    PureQ,
    runPureQ,

    -- * The @QuoteT@ monad transformer
    QuoteT,
    runQuoteT,
  )
where

import Control.Monad.Quote.Internal
import Data.Functor.Identity

-- | A pure variant of the 'QuoteT' monad transformer.
--
-- Useful to get an 'Language.Haskell.TH.Exp' out of a @'Quote' m => m 'Language.Haskell.TH.Exp'@ via 'runPureQ'.
type PureQ = QuoteT Identity

-- | Extract @a@ from @'PureQ' a@, where 'PureQ' is an instance of 'Quote'.
--
-- On GHC 9.0+, you can use this to extract an 'Language.Haskell.TH.Exp' out of a quotation bracket:
--
-- >>> runPureQ [| \x -> x + 5 |]
-- LamE [VarP x_0] (InfixE (Just (VarE x_0)) (VarE GHC.Num.+) (Just (LitE (IntegerL 5))))
runPureQ :: PureQ a -> a
runPureQ = runIdentity . runQuoteT
