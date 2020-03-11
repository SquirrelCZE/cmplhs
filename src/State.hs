{-# LANGUAGE ExistentialQuantification #-}

module State where

import Data.Typeable (typeOf)
import           Brick    as Br
import           SideProc (SideProcess)


data Target = Target {
        _cmd :: String
    }

formatTarget :: Int -> Target -> String
formatTarget i t = show i ++ " - " ++ (_cmd t)

data SharedState = SharedState {
    _targets         :: [Target],
    _active_target :: Int,
    _compile_process :: Maybe SideProcess
}

data Viewports = ErrViewport | StdViewport deriving (Eq, Ord, Show)

class AppMode a where
    renderMode :: SharedState -> a -> Br.Widget Viewports
    stepMode :: SharedState -> a -> IO (SharedState, a)
    modeOnExec :: a -> a
    modeOnExec a = a
    modeName :: a -> String

data Mode = forall a . AppMode a => MkMode a

packMode :: AppMode a => a -> Mode
packMode = MkMode

data AppState = AppState {
    _modes       :: [Mode],
    _active_mode :: Int,
    _shared      :: SharedState
}

