{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Control.Monad.ST.PromptControl (
    -- * Prompt/Control
    PromptTag (..),
    newPromptTag,
    prompt,
    control0,
    abort,
    -- * MagicHash variants
    PromptTag#,
    newPromptTag#,
    prompt#,
    control0#,
    abort#,
) where

import           GHC.Exts (PromptTag#, State#, unsafeCoerce#)
import qualified GHC.Exts as GHC (control0#, newPromptTag#, prompt#)
import           GHC.ST   (ST (..))

import Control.Monad.IO.PromptControl (PromptTag (..))

-------------------------------------------------------------------------------
-- Prompt/Control
-------------------------------------------------------------------------------

newPromptTag :: ST s (PromptTag a)
newPromptTag =
    ST (\s -> case newPromptTag# s of
        (# s', tag #) -> (# s, PromptTag tag #))

prompt :: PromptTag a -> ST s a -> ST s a
prompt (PromptTag tag) (ST m) = ST (prompt# tag m)

control0 :: PromptTag a -> ((ST s b -> ST s a) -> ST s a) -> ST s b
control0 (PromptTag tag) f =
    ST (control0# tag (\k -> case f (\(ST a) -> ST (k a)) of ST b -> b))

abort :: PromptTag a -> ST s a -> ST s b
abort (PromptTag tag) (ST a) =
    ST (abort# tag a)

-------------------------------------------------------------------------------
-- Raw
-------------------------------------------------------------------------------

newPromptTag# :: State# s -> (# State# s, PromptTag# a #)
newPromptTag# = unsafeCoerce# GHC.newPromptTag#

prompt# :: PromptTag# a -> (State# s-> (# State# s, a #)) -> State# s -> (# State# s, a #)
prompt# = unsafeCoerce# GHC.prompt#

control0# :: PromptTag# a -> (((State# s -> (# State# s, p #)) -> State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)) -> State# s -> (# State# s, p #)
control0# = unsafeCoerce# GHC.control0#

-------------------------------------------------------------------------------
-- MagicHash variants
-------------------------------------------------------------------------------

abort# :: PromptTag# a
       -> (State# s -> (# State# s, a #))
       -> State# s -> (# State# s, b #)
abort# pt a s = control0# pt (\_ -> a) s
