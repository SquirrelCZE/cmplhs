module Keys where

import           Brick        as Br
import           Data.List    (sort)
import qualified Graphics.Vty as Vt
import           State        (AppState, Viewports)

data KeyBind = KeyBind {
   _key   :: Vt.Key,
   _mods  :: [Vt.Modifier],
   _desc  :: String,
   _event :: AppState -> Br.EventM Viewports (Br.Next AppState)
}

mkBind :: Vt.Key -> [Vt.Modifier] -> String -> (AppState -> Br.EventM Viewports (Br.Next AppState)) -> KeyBind
mkBind = KeyBind

handleKeyEvent :: AppState -> Vt.Key -> [Vt.Modifier] -> [KeyBind] -> Br.EventM Viewports (Br.Next AppState)
handleKeyEvent as _ _ [] = Br.continue as
handleKeyEvent as key mods (bind : other) = if key == _key bind && sort (_mods bind) == sort mods
            then _event bind as
            else handleKeyEvent as key mods other

getKeyHelp :: KeyBind -> String
getKeyHelp (KeyBind key mod desc event) = match_key key ++ match_mod mod  ++ " - " ++ desc
    where
        match_key (Vt.KChar ch) = [ch]
        match_key k             = show k
        match_mod [] = []
        match_mod m  = " - " ++ show m
