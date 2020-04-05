

module Targets where

import qualified Data.Makefile as Mk
import Data.Makefile.Parse (parseMakefile)
import Data.Text (unpack)
import           Data.Maybe    (catMaybes)

import Debug.Trace (trace)

newtype Target = Target {
        _cmd :: String
    }

formatTarget :: Int -> Target -> String
formatTarget i t = show i ++ " - " ++ _cmd t

entryToTarget :: Mk.Entry -> Maybe Target
entryToTarget (Mk.Rule (Mk.Target target_name) _ _) = case (unpack target_name) of
        ".PHONY" -> Nothing
        name -> Just $ Target{_cmd = "make " ++ name}
entryToTarget _ = Nothing

loadTargets :: IO [Target]
loadTargets = do
    eithr <- parseMakefile
    case eithr of
        Right (Mk.Makefile entries) -> return $  catMaybes $ entryToTarget <$> entries
        Left a -> return []
