-- This doesn't actually work and it breaks the hinting
-- however, HLint reports on itself and Neil has refused
-- to provide a faculty for ignoring specific files from
-- the command line, so this has to stay disabled.
-- https://github.com/ndmitchell/hlint/issues/117#issuecomment-307928186

module HLint where

import "hint" HLint.Builtin.All
import "hint" HLint.Default
import "hint" HLint.Dollar

ignore "Use String"
ignore "Redundant do"
ignore "Use camelCase"
