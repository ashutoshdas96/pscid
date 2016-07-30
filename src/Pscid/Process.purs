module Pscid.Process where

import Prelude
import Ansi.Codes (Color(Green, Red))
import Control.Bind ((=<<))
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (catchException, Error)
import Control.Monad.Eff.Ref (readRef, modifyRef, newRef, REF)
import Data.Array (uncons)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.String (split)
import Node.ChildProcess (Exit(BySignal, Normally), onExit, stderr, stdout, defaultSpawnOptions, spawn, CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import Pscid.Console (logColored)

execCommand
  ∷ ∀ e
  . String
  → String
  → Eff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE, ref :: REF | e) Unit
execCommand name command = do
  log ("Running: \"" <> command <> "\"")
  execCommand' name command err k
  where
    k {code, output} = case code of
      0 -> logColored Green (name <> " successful!")
      x -> do
        log output
        logColored Red (name <> " errored with code: " <> show code)
    err _ = log (name <> " threw an exception")

type CommandResult =  {code :: Int, output :: String}

execCommandAff
  :: forall e
  . String
  → String
  → Aff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE, ref :: REF | e) CommandResult
execCommandAff name command =
  makeAff (execCommand' name command)

execCommand'
  ∷ ∀ e
  . String
  → String
  -> (Error -> Eff ( cp ∷ CHILD_PROCESS, ref :: REF | e) Unit)
  -> (CommandResult -> Eff ( cp ∷ CHILD_PROCESS, ref :: REF | e) Unit)
  → Eff ( cp ∷ CHILD_PROCESS, ref :: REF | e) Unit
execCommand' name command err k = do
  let cmd = unsafePartial fromJust (uncons (split " " command))
  output ← newRef ""
  cp ← spawn cmd.head cmd.tail defaultSpawnOptions

  let stout = stdout cp
      sterr = stderr cp

  catchException err $ onDataString stout UTF8 \s →
    modifyRef output (_ <> s) $> unit

  catchException err $ onDataString sterr UTF8 \s →
    modifyRef output (_ <> s) $> unit

  onExit cp \e → case e of
    Normally code → do
      output' <- readRef output
      k {code, output: output'}
    BySignal _       → k {code: 1, output: ""}
