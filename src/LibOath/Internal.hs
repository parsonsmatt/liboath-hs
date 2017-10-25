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

The functions are undocumented here. They are mirrored from <http://www.nongnu.org/oath-toolkit/liboath-api/liboath-oath.html the library documentation>.

-}
module LibOath.Internal where

import qualified Language.C.Inline as C
import qualified Foreign.C.String as C
import Foreign.Ptr (Ptr)

C.include "<oath.h>"

_OATH_OK :: C.CInt
_OATH_OK = 0

oath_init :: IO C.CInt
oath_init = [C.exp|int { oath_init() }|]

oath_done :: IO C.CInt
oath_done = [C.exp|int { oath_done() }|]

oath_check_version :: C.CString -> IO C.CString
oath_check_version minVersion = 
    [C.exp|const char * { oath_check_version($(const char * minVersion)) }|]

oath_strerror :: C.CInt -> IO C.CString
oath_strerror err = [C.exp|const char * { oath_strerror($(int err)) }|]

oath_base32_decode :: C.CString -> C.CSize -> Ptr C.CString -> Ptr C.CSize -> IO C.CInt
oath_base32_decode in_ inlen out outlen =
    [C.exp|int { 
        oath_base32_decode($(const char * in_), $(size_t inlen), $(char ** out), $(size_t * outlen)) 
    }|]

oath_base32_encode :: C.CString -> C.CSize -> Ptr C.CString -> Ptr C.CSize -> IO C.CInt
oath_base32_encode in_ inlen out outlen =
    [C.exp|int { 
        oath_base32_encode($(const char * in_), $(size_t inlen), $(char ** out), $(size_t * outlen)) 
    }|]

oath_totp_generate2 
    :: C.CString -> C.CSize -> C.CTime -> C.CUInt -> C.CTime -> C.CUInt -> C.CInt -> C.CString -> IO C.CInt
oath_totp_generate2 secret secretLen now timeStepSize startOffset digits flags outputOtp =
    [C.exp|int {
        oath_totp_generate2(
            $(const char * secret), 
            $(size_t secretLen),
            $(time_t now),
            $(unsigned int timeStepSize),
            $(time_t startOffset),
            $(unsigned int digits),
            $(int flags),
            $(char * outputOtp)
        ) 
    }|]
