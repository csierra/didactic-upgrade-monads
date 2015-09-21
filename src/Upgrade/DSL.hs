module Upgrade.DSL where

import Control.Monad
import Text.Printf
import Upgrade.Core

whenC :: DBCmd Bool v -> DBCmd () v -> DBCmd () v
whenC p cmd = do x <- p
                 when x cmd

tableExists :: String -> DBCmd Bool v
tableExists tab = fmap (not . null) $ query sqlQuery
    where sqlQuery =
              printf "SELECT name FROM sqlite_master WHERE type = 'table' AND name = '%s'" tab

addColumn :: String -> String -> String -> DBCmd () v
addColumn tab col typ =
    sql $ printf "ALTER TABLE %s ADD COLUMN %s %s" tab col typ
