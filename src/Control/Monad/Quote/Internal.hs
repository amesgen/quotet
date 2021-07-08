{-# LANGUAGE CPP #-}

-- | Internal module, no stability guarantees
module Control.Monad.Quote.Internal
  ( Quote (..),
    QuoteT (..),
    runQuoteT,
  )
where

import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.RWS.Class
import Control.Monad.State.Strict
import Language.Haskell.TH.Syntax (Uniq, mkNameU)
import Language.Haskell.TH.Syntax.Compat (Quote (..))

#ifdef MIN_VERSION_exceptions
import Control.Monad.Catch
#endif

#ifdef MIN_VERSION_semigroupoids
import Data.Functor.Apply (Apply (..))
import Data.Functor.Bind (Bind (..))
#endif

#ifdef MIN_VERSION_monad_control
import Control.Monad.Base
import Control.Monad.Trans.Control
#endif

-- $setup
-- >>> import Language.Haskell.TH (Exp)
-- >>> import Control.Monad.Reader

-- | The 'QuoteT' monad transformer. Also see 'runQuoteT'.
--
-- Useful to add a 'Quote' instance to any monad transformer stack.
--
-- Internally, this is a newtype of @'StateT' 'Uniq'@.
newtype QuoteT m a = QuoteT {unQuoteT :: StateT Uniq m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadIO,
      Alternative,
      MonadPlus,
      MonadFix,
      MonadTrans,
      MonadReader r,
      MonadWriter w,
      MonadRWS r w s,
      MonadCont,
      MonadError e
    )

#ifdef MIN_VERSION_exceptions
  deriving newtype (MonadThrow, MonadCatch, MonadMask)
#endif

#ifdef MIN_VERSION_semigroupoids
  deriving newtype (Apply)
#endif

#ifdef MIN_VERSION_monad_control
  deriving newtype (MonadBase b, MonadBaseControl b, MonadTransControl)
#endif

instance MonadState s m => MonadState s (QuoteT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance Monad m => Quote (QuoteT m) where
  newName s = QuoteT . state $ \i -> (mkNameU s i, i + 1)

#ifdef MIN_VERSION_semigroupoids
instance Bind m => Bind (QuoteT m) where
  QuoteT sma >>- f = QuoteT $ sma >>- unQuoteT . f
#endif

-- | Extract @m a@ from @'QuoteT' m a@, where @'QuoteT' m@ is always an instance of 'Quote'.
--
-- On GHC 9.0+, you can use this to extract an 'Language.Haskell.TH.Exp' out of a quotation bracket:
--
-- >>> import qualified Language.Haskell.TH.Lib as TH
-- >>> :{
-- let mExp :: (Quote m, MonadReader String m) => m Exp
--     mExp = [| \a -> a <> $(TH.stringE =<< ask) |]
--     exp :: Exp
--     exp = flip runReader " world" . runQuoteT $ mExp
--  in exp
-- :}
-- LamE [VarP a_0] (InfixE (Just (VarE a_0)) (VarE GHC.Base.<>) (Just (LitE (StringL " world"))))
runQuoteT :: Monad m => QuoteT m a -> m a
runQuoteT = flip evalStateT 0 . unQuoteT
