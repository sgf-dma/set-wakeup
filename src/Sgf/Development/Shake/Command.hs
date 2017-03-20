{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Sgf.Development.Shake.Command
Description : Wrapper and type for running privileged commands.
Maintainer  : sgf.dma@gmail.com

-}
module Sgf.Development.Shake.Command
    ( Priv (..)
    , commandAs
    , commandAsRoot
    , privCmd
    , rootCmd
    )
  where

import Data.Either
import Development.Shake
import Development.Shake.Command


-- | Type for a command, which should run from different user.
newtype Priv a      = Priv a
  deriving (Show)

-- | 'command', which should run as different user. Requires a /non-empty/
-- username. To run as @root@ without specifying username, use
-- 'commandAsRoot'.
commandAs :: CmdResult r => String              -- ^ User name to run command as.
             -> Priv [Either CmdOption String]  -- ^ Command to run.
             -> Action r
commandAs []   _        = fail "Error, empty user name for privileged command."
commandAs user (Priv x) = case partitionEithers x of
    (opts, xs@(_ : _))  -> command opts "sudo" ("-u" : user : "--" : xs)
    _                   -> error "Error, no executable given."

-- | Just a shortcut for
--
--  > commandAs "root"
commandAsRoot :: CmdResult r => Priv [Either CmdOption String] -> Action r
commandAsRoot       = commandAs "root"

-- | 'cmd', which should run as different user. Requires a /non-empty/
-- username. To run as @root@ without specifying username, use 'rootCmd'.
--
-- This function may be useful for running privileged command directly in 'IO'
-- monad instead of 'Action' monad, because there is an
--
--  > instance CmdResult r => CmdArguments (IO r)
--
-- If i want to enforce, that an executable to run by @sudo@, is given, i
-- should define 'privCmd' like
--
--  > privCmd :: (Arg a, CmdArguments b) => String -> a -> b
--
-- but 'Arg' is not exported. So i let @sudo@ fail, when there is no command to run.
privCmd :: CmdArguments b => String -> b
--privCmd :: (Arg a, CmdArguments b) => String -> a -> b
privCmd []          = error "Error, empty user name for privileged command."
privCmd u           = cmd ["sudo", "-u", u]

-- | Just a shortcut for
--
--  > privCmd root
rootCmd :: CmdArguments b => b
--rootCmd :: (Arg a, CmdArguments b) => a -> b
rootCmd             = privCmd "root"

