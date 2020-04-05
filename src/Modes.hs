{-# LANGUAGE InstanceSigs #-}

module Modes where

import           Brick         as Br
import           Data.Maybe    (catMaybes)
import           GccJsonParser (ErrorCodeMark (..), ErrorFixIt (..),
                                ErrorKind (..), ErrorLocation (..),
                                ErrorPathItem (..), GccError (..),
                                extractGccErrors)
import           Keys          (KeyBind (..), getKeyHelp)
import           SideProc      (SideProcess (..), readLines)
import              Targets (formatTarget)
import           State         (AppMode, AppState, SharedState, Viewports (..),
                                 modeName, modeOnExec, modeScroll,
                                renderMode, stepMode, _active_target,
                                _compile_process, _err_lines, _std_lines,
                                _targets)
import           Util          (hLimitMax, errorAttr)

data ErrorMode = ErrorMode {
}

instance AppMode ErrorMode where
    renderMode :: SharedState -> ErrorMode -> Br.Widget Viewports
    renderMode st _ = Br.viewport ErrViewport Br.Vertical $ Br.vBox (hLimitMax . strWrap <$> reverse (_err_lines st))

    modeName _ = "STDERR"

    modeScroll :: ErrorMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ c as = Br.vScrollBy (Br.viewportScroll ErrViewport) c >> Br.continue as

data StdMode = StdMode {
}

instance AppMode StdMode where
    renderMode :: SharedState -> StdMode -> Br.Widget Viewports
    renderMode st _ = Br.viewport StdViewport Br.Vertical $ Br.vBox (hLimitMax . strWrap <$> reverse (_std_lines st))

    modeName _ = "STDOUT"

    modeScroll :: StdMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ c as = Br.vScrollBy (Br.viewportScroll StdViewport) c >> Br.continue as

newtype InfoMode = InfoMode {
    _keys :: [KeyBind]
}

instance AppMode InfoMode where
    renderMode :: SharedState -> InfoMode -> Br.Widget Viewports
    renderMode st (InfoMode keys) = Br.vBox $ fmap strWrap $ (empty : targets ) ++ (empty :key_help)
        where
            empty =  "   "
            single_target (i, t) = (if i == _active_target st then " * " else "   ") ++ formatTarget i t
            targets = "Targets: " : fmap single_target ( zip [0..] (_targets st))
            single_key k = "   " ++ getKeyHelp k
            key_help = "Key bindings: " : fmap single_key keys

    modeName _ = "INFO"

    modeScroll :: InfoMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ _ = Br.continue

data GccJsonMode = GccJsonMode {}

instance AppMode GccJsonMode where
    renderMode :: SharedState -> GccJsonMode -> Br.Widget Viewports
    renderMode st _ = Br.vBox (gcc_error_to_widget <$> extractGccErrors (reverse $ _err_lines st))
        where
            gcc_error_to_widget :: GccError -> Br.Widget Viewports
            gcc_error_to_widget (FailedGccError e line) = 
                Br.vBox [strWrap $ "Failed to parse gcc json error due to: " ++ e, strWrap line]
            gcc_error_to_widget (GccError kind msg mb_opt locations fixits path children ) =
                Br.vBox $ (strWrap "  ") : headline kind msg mb_opt :
                (loc_to_w <$> locations) ++ 
                (fixit_to_w <$> fixits) ++ 
                (path_to_w <$> path) ++ 
                (Br.padLeft (Br.Pad 5) . gcc_error_to_widget <$> children)

            headline :: ErrorKind -> String -> Maybe String -> Br.Widget Viewports
            headline kind msg mb_opt = 
                Br.vBox $ [ Br.hBox $ [start] ++ catMaybes [ (Br.padLeftRight 5 . strWrap) <$> mb_opt]  , Br.padLeftRight 5 $ strWrap msg ]
                where 
                    start = case kind of
                        Warning -> strWrap " warning "
                        Error -> withAttr errorAttr $ strWrap " error "
                        Note -> strWrap " note "
                        FatalError ->  withAttr errorAttr $ strWrap " fatal error "

            loc_to_w :: ErrorLocation -> Br.Widget Viewports
            loc_to_w (ErrorLocation caret mb_finish mb_start mb_label) =
                Br.vBox $ 
                code_mark_to_w caret : 
                catMaybes [ code_mark_to_w <$> mb_finish, code_mark_to_w <$> mb_start, strWrap <$> mb_label ]

            code_mark_to_w :: ErrorCodeMark -> Br.Widget Viewports
            code_mark_to_w (ErrorCodeMark col file line) = strWrap $ file ++ ":" ++ show line ++ ":" ++ show col

            fixit_to_w :: ErrorFixIt -> Br.Widget Viewports
            fixit_to_w (ErrorFixIt start next string) = Br.hBox [strWrap string, code_mark_to_w start, code_mark_to_w next]

            path_to_w :: ErrorPathItem -> Br.Widget Viewports
            path_to_w (ErrorPathItem desc loc func _) = Br.vBox [ head, code_mark_to_w loc ]
                where
                    head = Br.hBox [ strWrap func, strWrap desc ]


    modeName _ = "GCC_JSON"

    modeScroll :: GccJsonMode -> Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
    modeScroll _ _  = Br.continue
