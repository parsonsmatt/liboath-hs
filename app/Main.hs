module Main where

import Control.Monad (when)
import qualified Foreign.Marshal.Alloc as Alloc
import System.Environment (getArgs)
import qualified Foreign.C.String as C
import qualified Foreign.C.Types as C
import Data.Time.Clock.POSIX
import Data.Function (fix)
import Foreign.Storable (peek)

import LibOath.Internal

-- this is basically just copying over the main function from oathtool.c
main :: IO ()
main = do
    (input : _) <- getArgs

    rc <- oath_init
    when (rc /= _OATH_OK) (error "liboath initialization failed")
    cinput <- C.newCAString input
    let inputlen = length input
    secretPtr <- Alloc.malloc
    secretLenPtr <- Alloc.malloc

    rc <- oath_base32_decode cinput (fromIntegral inputlen) secretPtr secretLenPtr

    when (rc /= _OATH_OK) (error "base32 decoding failed")

    secretDecoded <- C.peekCString =<< peek secretPtr

    let movingFactor = 0
        digits = 6
        window = 0
    otpPtr <- C.newCString (replicate (fromIntegral digits + 1) ' ')

    -- assuming totp given
    now <- fromIntegral . floor <$> getPOSIXTime
    -- assume no command args passed
    let whenTime = now
        t0 = 0
        timeStepSize = 0
        totpFlags = 0

    -- assume one input given, so generate_otp returns true
    secretLen <- peek secretLenPtr
    secret <- peek secretPtr
    flip fix 0 $ \go iter -> do
        rc <- oath_totp_generate2
            secret secretLen
            (whenTime + iter * timeStepSize)
            (fromIntegral (case timeStepSize of C.CTime i -> i))
            t0
            digits
            totpFlags
            otpPtr
        when (rc /= _OATH_OK) (error "generating passcode failed")
        otp <- C.peekCString otpPtr
        putStrLn otp
        let iter' = iter + 1
        when (window - iter' > 0) (go iter')

    oath_done
    pure ()
