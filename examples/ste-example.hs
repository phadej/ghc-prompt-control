{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RoleAnnotations        #-}
module Main (
    main,
) where

import Control.Monad             (ap, liftM)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.ST          (ST, runST)
import Data.List.NonEmpty        (NonEmpty (..), cons, uncons)
import Unsafe.Coerce             (unsafeCoerce)

import Test.Tasty       (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.ST.PromptControl

-------------------------------------------------------------------------------
-- STE monad
-------------------------------------------------------------------------------

-- | Isomorphic to @ExceptT e (ST s)@
newtype STE s e a = STE { unSTE :: Handlers s e -> ST s a }
type role STE nominal representational representational

data Handlers s e where
    TopHandler :: PromptTag (Either e a) -> Handlers s e
    Handler    :: PromptTag a -> (e -> STE s e a) -> Handlers s e -> Handlers s e

instance Functor (STE s e) where
    fmap = liftM

instance Applicative (STE s e) where
    pure x = STE (\_ -> return x)
    (<*>) = ap

instance Monad (STE s e) where
    return = pure

    m >>= k = STE $ \hs -> do
        x <- unSTE m hs
        unSTE (k x) hs

instance MonadError e (STE s e) where
    throwError = throwSTE
    catchError = catchSTE

runSTE :: (forall s. STE s e a) -> Either e a
runSTE m = runST $ do
    tag <- newPromptTag
    prompt tag (fmap Right (case m of STE m' -> m' (TopHandler tag)))

throwSTE :: forall e s a. e -> STE s e a
throwSTE e = STE $ \hs -> case hs of
    TopHandler tag ->
        abort tag (return (Left e))
    Handler tag f hs ->
        abort tag (unSTE (f e) hs)

catchSTE :: STE s e a -> (e -> STE s e a) -> STE s e a
catchSTE (STE m) handler =
    STE $ \hs -> do
        tag <- newPromptTag
        prompt tag (m (Handler tag handler hs))

-------------------------------------------------------------------------------
-- Main tests
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "STE"
    [ testCase "ok" $ do
          runSTE (return 'x') @?= (Right 'x' :: Either Int Char)

    , testCase "throws" $ do
          runSTE (throwSTE 10 >> return 'x') @?= (Left 10 :: Either Int Char)

    , testCase "catch" $ do
        let action :: STE s Int Char
            action = catchSTE
                (return 'a' >> throwSTE 10 >> return 'b')
                (\e -> if e == 10 then return 'x' else return 'y')

        runSTE action @?= Right 'x'

    , testCase "throw-catch-1" $ do
        let action :: STE s Int Char
            action = catchSTE
                (return 'a' >> throwSTE 10 >> return 'b')
                (\_ -> throwSTE 11)

        runSTE action @?= Left 11

    , testCase "throw-catch-2" $ do
        let action1 :: STE s Int Char
            action1 = catchSTE
                (return 'a' >> throwSTE 10 >> return 'b')
                (\_ -> throwSTE 11)

        let action2 :: STE s Int Char
            action2 = catchSTE action1
                (\_ -> return 'z')

        runSTE action2 @?= Right 'z'

    ]
