module Main where

import Lens.Micro (over, ix, (&), (%~))
import           Brick                  as Br
import           Brick.BChan            (newBChan, writeBChan)
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (digitToInt)
import           Data.List              (intercalate)
import           Data.Maybe             (isJust)
import qualified Graphics.Vty           as Vt
import           Keys                   (KeyBind, getKeyHelp, handleKeyEvent,
                                         mkBind)
import           Modes                  (ErrorMode (..), GccJsonMode (..),
                                         InfoMode (..), StdMode (..))
import Targets (Target(..),loadTargets, _cmd)
import           SideProc               (runSideProcess)
import           State                  (AppState (..), Mode (..),
                                         SharedState (..),
                                         Viewports, modeName, modeOnExec,
                                         modeScroll, packMode, renderMode,
                                         stepMode, stepSharedState,
                                         _modes, _shared, _targets)
import Util (errorAttr, activeAttr)

data Tick = Tick

drawTopLine :: Mode -> SharedState -> Br.Widget Viewports
drawTopLine (MkMode m) st = Br.hBox [pad_to 10 ( modeName m)]
    where
        pad_to :: Int -> String -> Br.Widget Viewports
        pad_to c str = Br.padLeftRight ((c - length str) `div` 2 ) $ strWrap str

        -- opt_running = [withAttr activeAttr $ pad_to 15 "RUNNING"| isJust $ _compile_process st]

drawUI :: AppState -> [Br.Widget Viewports]
drawUI (AppState modes active_mode shared) = [Br.vBox [ drawTopLine mode shared, mode_widget mode]]
    where
        mode = modes !! active_mode
        mode_widget (MkMode m) = renderMode shared m

step :: AppState -> IO AppState
step as = do
        newshared <- stepSharedState (_shared as)
        (newst, newmods) <- proc_module newshared (_modes as)
        return AppState {_modes = newmods, _active_mode = _active_mode as, _shared = newst}
    where
        proc_module :: SharedState -> [Mode] -> IO (SharedState, [Mode])
        proc_module st [] = return (st, [])
        proc_module st ((MkMode m):xm) = do
                (newst, newmods) <- proc_module st xm
                (actualst, actualmode) <- stepMode newst m
                return (actualst, packMode actualmode : newmods)

changeMode :: Int -> AppState -> AppState
changeMode c (AppState modes _ shared) = AppState modes new_mode shared
    where
        new_mode = (c - 1) `mod` length modes

changeTarget :: AppState -> AppState
changeTarget (AppState modes active_mode shared) = AppState modes active_mode new_shared
    where
        new_shared = SharedState (_targets shared) new_target (_compile_process shared) (_err_lines shared) (_std_lines shared)
        new_target = (_active_target shared + 1) `mod` length ( _targets shared)

runTarget :: AppState -> IO AppState
runTarget (AppState modes active_mode shared) = do
    sp <- runSideProcess $ _cmd (_targets shared !! _active_target shared)
    return AppState {
            _modes = fmap match_mode modes,
            _active_mode = active_mode,
            _shared = SharedState {
                _targets = _targets shared,
                _active_target = _active_target shared,
                _compile_process = Just sp,
                _err_lines = [],
                _std_lines = []
            }
        }

    where
        match_mode (MkMode m) = MkMode $ modeOnExec m


keyBinds :: [KeyBind]
keyBinds = [mkBind (Vt.KChar 'q') [] "quit" Br.halt,
    mkBind (Vt.KChar 't') [] "change target" (Br.continue .changeTarget),
    mkBind (Vt.KChar 'r') [] "execute target" exec_target,
    mkBind Vt.KDown [] "scroll down" (scroll_view 1),
    mkBind Vt.KUp [] "scroll up lines" (scroll_view (-1))
    ] ++ num_binds ['1', '2', '3', '4']
    where
        num_binds [] = []
        num_binds (x:xs) = mkBind (Vt.KChar x) [] ("switch to mode " ++ [x]) ( Br.continue . changeMode (digitToInt x)) : num_binds xs

        exec_target as = liftIO (runTarget as) >>= Br.continue
        scroll_view :: Int -> AppState -> Br.EventM Viewports (Br.Next AppState)
        scroll_view c (AppState mods activei shrd) = Br.continue $ AppState {
               _modes = (mods & (ix activei) %~ scroll),
               _active_mode = activei,
               _shared = shrd
            }
            where
                scroll :: Mode -> Mode 
                scroll (MkMode m) = MkMode $ modeScroll m c

handleEvent :: AppState -> Br.BrickEvent Viewports Tick -> Br.EventM Viewports (Br.Next AppState)
handleEvent as (Br.AppEvent Tick) = liftIO (step as) >>= Br.continue
handleEvent as (Br.VtyEvent (Vt.EvKey key mods)) = handleKeyEvent as key mods keyBinds
handleEvent as _ = Br.continue as

initMainState :: IO AppState
initMainState = do
    targets <- loadTargets
    return AppState {
        _modes = [
           packMode $ ErrorMode  0,
           packMode $ StdMode 0,
            packMode $ GccJsonMode 0,
            packMode $ InfoMode keyBinds
            ],
        _active_mode = 0,
        _shared = SharedState {
            _targets = targets,
            _active_target = 0,
            _compile_process = Nothing,
                _err_lines = [],
                _std_lines = []
        }
        }


app :: Br.App AppState Tick Viewports
app = Br.App
    { appDraw = drawUI,
    appChooseCursor = Br.neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return,
    appAttrMap = const (Br.attrMap Vt.defAttr [
            (activeAttr, Vt.black `Br.on` Vt.blue),
            (errorAttr, Br.fg Vt.red `Vt.withStyle` Vt.bold)
        ])
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 300000 -- decides how fast your app ticks
  g <- initMainState
  let buildVty = Vt.mkVty Vt.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app g
