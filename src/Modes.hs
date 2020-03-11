{-# LANGUAGE InstanceSigs #-}

module Modes where

import           Brick    as Br
import           Keys     (KeyBind (..), getKeyHelp)
import           SideProc (SideProcess (..), readLines)
import           State    (AppMode, AppState, SharedState, Viewports (..), renderMode, formatTarget, modeOnExec, modeName, modeScroll,
                           stepMode, _compile_process, _targets, _active_target)
import           Util     (hLimitMax)

data ErrorMode = ErrorMode {
    _err_lines :: [String]
}

instance AppMode ErrorMode where
    renderMode :: SharedState -> ErrorMode -> Br.Widget Viewports
    renderMode _ em = Br.viewport ErrViewport Br.Vertical $ Br.vBox (fmap (hLimitMax . strWrap) $ reverse (_err_lines em))

    stepMode :: SharedState -> ErrorMode -> IO (SharedState, ErrorMode)
    stepMode st em = case (_compile_process st) of
        Nothing -> return (st, em)
        Just (SideProcess _ stderr proc) -> do
            new_lines <- readLines stderr
            return (st, ErrorMode {_err_lines = reverse new_lines ++ (_err_lines em)})

    modeOnExec _ = ErrorMode []

    modeName _ = "STDERR"
    
    modeScroll :: ErrorMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ c as = (Br.vScrollBy (Br.viewportScroll ErrViewport) c) >> Br.continue as

data StdMode = StdMode {
    _std_lines :: [String]
}

instance AppMode StdMode where
    renderMode :: SharedState -> StdMode -> Br.Widget Viewports
    renderMode _ sm = Br.viewport StdViewport Br.Vertical $ Br.vBox (fmap (hLimitMax . strWrap) $ reverse (_std_lines sm))

    stepMode :: SharedState -> StdMode -> IO (SharedState, StdMode)
    stepMode st em = case (_compile_process st) of
        Nothing -> return (st, em)
        Just (SideProcess stdout _ proc) -> do
            new_lines <- readLines stdout
            return (st, StdMode {_std_lines = reverse new_lines ++ (_std_lines em)})
    
    modeOnExec _ = StdMode []

    modeName _ = "STDOUT"
    
    modeScroll :: StdMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ c as = (Br.vScrollBy (Br.viewportScroll StdViewport) c) >> Br.continue as

data InfoMode = InfoMode {
    _keys :: [KeyBind]
}

instance AppMode InfoMode where
    renderMode :: SharedState -> InfoMode -> Br.Widget Viewports
    renderMode st (InfoMode keys) = Br.vBox $ fmap strWrap $ (empty : targets ) ++ (empty :key_help)
        where
            empty =  "   " 
            single_target (i, t) = (if i == _active_target st then " * " else "   ") ++ formatTarget i t
            targets = "Targets: " : (fmap single_target $ zip [0..] (_targets st))
            single_key k = "   " ++ getKeyHelp k
            key_help = "Key bindings: " : fmap single_key keys

    stepMode :: SharedState -> InfoMode -> IO (SharedState, InfoMode)
    stepMode st im = return (st, im)

    modeName _ = "INFO"

    modeScroll :: InfoMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ _ as = Br.continue as
