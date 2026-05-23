{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
module Control.Monad.IO.PromptControl (
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

import GHC.Exts (PromptTag#, RealWorld, State#, control0#, newPromptTag#, prompt#)
import GHC.IO   (IO (..))

-------------------------------------------------------------------------------
-- Prompt/Control
-------------------------------------------------------------------------------

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag =
    IO (\s -> case newPromptTag# s of
        (# s', tag #) -> (# s, PromptTag tag #))

prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag tag) (IO m) = IO (prompt# tag m)

control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag tag) f =
    IO (control0# tag (\k -> case f (\(IO a) -> IO (k a)) of IO b -> b))

abort :: PromptTag a -> IO a -> IO b
abort (PromptTag tag) (IO a) =
    IO (abort# tag a) 

{-
-------------------------------------------------------------------------------
-- Shift/Reset
-------------------------------------------------------------------------------

reset :: PromptTag a -> IO a -> IO a
reset = prompt

shift :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
shift tag f = control0 tag (\k -> reset tag (f (\m -> reset tag (k m))))
-}

-------------------------------------------------------------------------------
-- MagicHash variants
-------------------------------------------------------------------------------

abort# :: PromptTag# a
       -> (State# RealWorld -> (# State# RealWorld, a #))
       -> State# RealWorld -> (# State# RealWorld, b #)
abort# pt a s = control0# pt (\_ -> a) s
