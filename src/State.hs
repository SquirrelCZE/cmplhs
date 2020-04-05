{-# LANGUAGE ExistentialQuantification #-}

module State where

import           Brick         as Br
import           Data.Typeable (typeOf)
import           SideProc      (SideProcess (..), readLines)
import           System.IO     (hIsEOF)
import Targets (Target(..))

data SharedState = SharedState {
    _targets         :: [Target],
    _active_target   :: Int,
    _compile_process :: Maybe SideProcess,
    _err_lines       :: [String],
    _std_lines       :: [String]
}

stepSharedState :: SharedState -> IO SharedState
stepSharedState st = case _compile_process st of
    Nothing -> return st
    Just sp -> do
        new_err_lines <- readLines $ _stderr_chan sp
        new_std_lines <- readLines $ _stdout_chan sp
        return SharedState { _targets = _targets st,  _active_target = _active_target st, _compile_process = Just sp, _err_lines = reverse new_err_lines ++ _err_lines st, _std_lines = reverse new_std_lines ++ _std_lines st }

data Viewports = ErrViewport | StdViewport | GccJsonViewport deriving (Eq, Ord, Show)

class AppMode a where
    renderMode :: SharedState -> a -> Br.Widget Viewports
    stepMode :: SharedState -> a -> IO (SharedState, a)
    stepMode b c = return (b,c)
    modeOnExec :: a -> a
    modeOnExec a = a
    modeName :: a -> String
    modeScroll :: a -> Int -> a

data Mode = forall a . AppMode a => MkMode a

packMode :: AppMode a => a -> Mode
packMode = MkMode

data AppState = AppState {
    _modes       :: [Mode],
    _active_mode :: Int,
    _shared      :: SharedState
}

