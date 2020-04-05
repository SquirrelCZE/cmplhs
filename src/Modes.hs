{-# LANGUAGE InstanceSigs #-}

module Modes where

import Data.List (intercalate)
import Lens.Micro (over, ix, (&), (%~))
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
    _errr_i :: Int
}

instance AppMode ErrorMode where
    renderMode :: SharedState -> ErrorMode -> Br.Widget Viewports
    renderMode st (ErrorMode err_i) = Br.viewport ErrViewport Br.Vertical $ Br.vBox $ wrapped & (ix err_i) %~ Br.visible
        where
            wrapped = hLimitMax . strWrap <$> reverse (_err_lines st)

    modeName _ = "STDERR"

    modeScroll :: ErrorMode -> Int -> ErrorMode
    modeScroll (ErrorMode i) c = ErrorMode (i + c)

data StdMode = StdMode {
    _std_i :: Int
}

instance AppMode StdMode where
    renderMode :: SharedState -> StdMode -> Br.Widget Viewports
    renderMode st (StdMode std_i) = Br.viewport StdViewport Br.Vertical $ Br.vBox $ wrapped & (ix std_i) %~ Br.visible
        where
            wrapped = hLimitMax . strWrap <$> reverse (_std_lines st)

    modeName _ = "STDOUT"

    modeScroll :: StdMode -> Int -> StdMode
    modeScroll (StdMode i) c = StdMode (i + c)

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

    modeScroll :: InfoMode -> Int -> InfoMode 
    modeScroll m _ = m

data GccJsonMode = GccJsonMode {
    _position :: Int
}

instance AppMode GccJsonMode where
    renderMode :: SharedState -> GccJsonMode -> Br.Widget Viewports
    renderMode st (GccJsonMode pos) = Br.viewport GccJsonViewport Br.Vertical $ Br.vBox $ widgets & (ix pos) %~ Br.visible
        where
            widgets = gcc_error_to_widget <$> extractGccErrors (reverse $ _err_lines st)

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
                marks ( caret : catMaybes [mb_start, mb_finish] ) : 
                catMaybes [  strWrap <$> mb_label ]
                where
                    marks :: [ErrorCodeMark] -> Br.Widget Viewports
                    marks locs 
                        | all (\x -> (_file x) == (_file $ head locs)) locs = merge_stringify locs 
                        | otherwise =  Br.vBox $ code_mark_to_w <$> locs

                    merge_stringify :: [ErrorCodeMark] -> Br.Widget Viewports
                    merge_stringify locs =  strWrap $ (_file $ head locs) ++ ":" ++  (intercalate "," $ (\x -> (show $ _line x) ++ ":" ++ (show $ _column x)) <$> locs)
                    

            code_mark_to_w :: ErrorCodeMark -> Br.Widget Viewports
            code_mark_to_w (ErrorCodeMark col file line) = strWrap $ file ++ ":" ++ show line ++ ":" ++ show col

            fixit_to_w :: ErrorFixIt -> Br.Widget Viewports
            fixit_to_w (ErrorFixIt start next string) = Br.hBox [strWrap string, code_mark_to_w start, code_mark_to_w next]

            path_to_w :: ErrorPathItem -> Br.Widget Viewports
            path_to_w (ErrorPathItem desc loc func _) = Br.vBox [ head, code_mark_to_w loc ]
                where
                    head = Br.hBox [ strWrap func, strWrap desc ]


    modeName _ = "GCC_JSON"

    modeScroll :: GccJsonMode -> Int -> GccJsonMode
    modeScroll (GccJsonMode i) c = GccJsonMode ( i + c )
