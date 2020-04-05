{-# LANGUAGE InstanceSigs #-}

module GccJsonParser where

import           Data.List  (isPrefixOf)
import           Data.Maybe (catMaybes)
import qualified Text.JSON  as Js

-- For convenience
-- from: https://stackoverflow.com/questions/17844223/json-parsing-in-haskell
(!) :: (Js.JSON a) => Js.JSObject Js.JSValue -> String -> Js.Result a
(!) = flip Js.valFromObj

mb :: (Js.JSON a) => Js.JSObject Js.JSValue -> String -> Js.Result [a]
mb obj key = case lookup key (Js.fromJSObject obj) of
    Nothing  -> Js.Ok []
    Just val -> Js.readJSON val

mbs :: Js.JSObject Js.JSValue -> String -> Js.Result (Maybe String)
mbs b key = case lookup key (Js.fromJSObject b) of
    Nothing -> Js.Ok Nothing
    Just val -> case (Js.readJSON :: Js.JSValue -> Js.Result String) val of
        Js.Ok v    -> Js.Ok $ Just v
        Js.Error e -> Js.Error e

showJSONMaybePair :: Js.JSON a => (String, Maybe a) -> Maybe (String, Js.JSValue)
showJSONMaybePair (a,b) = case b of
    Just val -> Just (a, Js.showJSON val)
    Nothing  -> Nothing

data ErrorKind = Warning | Error | Note | FatalError deriving (Show)

instance Js.JSON ErrorKind where
    readJSON :: Js.JSValue -> Js.Result ErrorKind
    readJSON (Js.JSString s) = case Js.fromJSString s of
        "warning" -> Js.Ok Warning
        "error"  -> Js.Ok Error
        "note" -> Js.Ok Note
        "fatal error" -> Js.Ok FatalError
        _ -> Js.Error $ "Failed to match error kind: " ++ Js.fromJSString s
    readJSON _ = Js.Error "Kind is not of json type string"

    showJSON :: ErrorKind -> Js.JSValue
    showJSON kind = case kind of
        Warning    -> Js.showJSON "warning"
        Error      -> Js.showJSON "error"
        Note       -> Js.showJSON "note"
        FatalError -> Js.showJSON "fatal error"

data ErrorCodeMark = ErrorCodeMark {
        _column :: Int,
        _file   :: String,
        _line   :: Int
    }
instance Js.JSON ErrorCodeMark where
    readJSON :: Js.JSValue -> Js.Result ErrorCodeMark
    readJSON (Js.JSObject obj) =
        ErrorCodeMark <$>
        obj ! "column" <*>
        obj ! "file" <*>
        obj ! "line"
    readJSON _ = Js.Error "Code mark is not of type json object"

    showJSON :: ErrorCodeMark -> Js.JSValue
    showJSON (ErrorCodeMark column file line) = Js.makeObj [
        ("column", Js.showJSON column),
        ("file", Js.showJSON file),
        ("line", Js.showJSON line)
        ]

-- TODO: this is not good :/
mbcm :: Js.JSObject Js.JSValue -> String -> Js.Result (Maybe ErrorCodeMark)
mbcm b c = case lookup c (Js.fromJSObject b) of
    Nothing -> Js.Ok Nothing
    Just val -> case (Js.readJSON :: Js.JSValue -> Js.Result ErrorCodeMark) val of
        Js.Ok v    -> Js.Ok $ Just v
        Js.Error e -> Js.Error e

data ErrorLocation = ErrorLocation {
        _caret     :: ErrorCodeMark,
        _mb_finish :: Maybe ErrorCodeMark,
        _mb_start  :: Maybe ErrorCodeMark,
        _mb_label  :: Maybe String
    }
instance Js.JSON ErrorLocation where
    readJSON :: Js.JSValue -> Js.Result ErrorLocation
    readJSON (Js.JSObject obj) =
        ErrorLocation <$>
        obj ! "caret" <*>
        obj `mbcm` "finish" <*>
        obj `mbcm` "start" <*>
        obj `mbs` "label"
    readJSON _ = Js.Error "Error location is not of type json object"

    showJSON :: ErrorLocation -> Js.JSValue
    showJSON (ErrorLocation caret mb_finish mb_start mb_label) = Js.makeObj $
        ("caret", Js.showJSON caret) :
        catMaybes [ showJSONMaybePair ("finish", mb_finish), showJSONMaybePair ("start", mb_start), showJSONMaybePair ("label", mb_label) ]

data ErrorFixIt = ErrorFixIt {
        _start  :: ErrorCodeMark,
        _next   :: ErrorCodeMark,
        _string :: String
    }
instance Js.JSON ErrorFixIt where
    readJSON :: Js.JSValue -> Js.Result ErrorFixIt
    readJSON (Js.JSObject obj) =
        ErrorFixIt <$>
        obj ! "start" <*>
        obj ! "next" <*>
        obj ! "string"
    readJSON _ = Js.Error "Fixit is not of type json object"

    showJSON :: ErrorFixIt -> Js.JSValue
    showJSON (ErrorFixIt start next str) = Js.makeObj [
            ("start", Js.showJSON start),
            ("next", Js.showJSON next),
            ("string", Js.showJSON str)
        ]

data ErrorPathItem = ErrorPathItem {
        _description :: String,
        _location    :: ErrorCodeMark,
        _function    :: String,
        _depth       :: Int
    }
instance Js.JSON ErrorPathItem where
    readJSON :: Js.JSValue -> Js.Result ErrorPathItem
    readJSON (Js.JSObject obj) =
        ErrorPathItem <$>
        obj ! "description" <*>
        obj ! "location" <*>
        obj ! "function" <*>
        obj ! "depth"
    readJSON _ = Js.Error "Path item is not of type json object"

    showJSON :: ErrorPathItem -> Js.JSValue
    showJSON (ErrorPathItem desc loc fun depth) = Js.makeObj [
            ("description", Js.showJSON desc),
            ("location", Js.showJSON loc),
            ("function", Js.showJSON fun),
            ("depth", Js.showJSON depth)
        ]

data GccError = GccError{
        _kind      :: ErrorKind,
        _msg       :: String,
        _mb_option :: Maybe String,
        _locations :: [ErrorLocation],
        _fixits    :: [ErrorFixIt],
        _path      :: [ErrorPathItem],
        _children  :: [GccError]
    } | FailedGccError {
        _msg       :: String,
        _json_line :: String
    }

instance Js.JSON GccError where
    readJSON :: Js.JSValue -> Js.Result GccError
    readJSON (Js.JSObject obj) =
        GccError <$>
        obj ! "kind" <*>
        obj ! "message" <*>
        obj `mbs` "option" <*>
        obj ! "locations" <*>
        obj `mb` "fixits" <*>
        obj `mb` "path" <*>
        obj `mb` "children"
    readJSON _ = Js.Error "GccError is not of type json object"

    showJSON :: GccError -> Js.JSValue
    showJSON (GccError kind msg opt loc fixits path child) = Js.makeObj $ [
            ("kind", Js.showJSON kind),
            ("message", Js.showJSON msg),
            ("locations", Js.showJSON loc),
            ("fixits", Js.showJSON fixits),
            ("path", Js.showJSON path),
            ("children", Js.showJSON child)
        ] ++ catMaybes [showJSONMaybePair ("option", opt)]

extractGccErrors :: [String] -> [GccError]
extractGccErrors lines = concat $ catMaybes $( parse_line <$> lines )
    where
        parse_line :: String -> Maybe [GccError]
        parse_line line =
            if "[" `isPrefixOf` line
            then case Js.decode line of
                Js.Ok gcc_e       -> Just gcc_e
                Js.Error failed_e -> Just [FailedGccError failed_e line]
            else Nothing



