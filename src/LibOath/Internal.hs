{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


{-|
Module      : LibOath.Internal
Description : Exports the C code functions. Use at your own risk!
Copyright   : (c) Matt Parsons, 2017
License     : GPL-3
Maintainer  : parsonsmatt@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the C functions called via "inline-c". Import these to use at your own risk.
-}
module LibOath.Internal where

import qualified Language.C.Inline as C
import qualified Foreign.C.String as C

C.include "<oath.h>"

oath_init :: IO C.CInt
oath_init = [C.exp|int { oath_init() }|]

oath_done :: IO C.CInt
oath_done = [C.exp|int { oath_done() }|]

oath_check_version :: C.CString -> IO C.CString
oath_check_version minVersion = 
    [C.exp|const char * { oath_check_version($(const char * minVersion)) }|]


