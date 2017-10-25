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

-- int
-- main (int argc, char *argv[])
-- {
--   struct gengetopt_args_info args_info;
--   char *secret;
--   size_t secretlen = 0;
--   int rc;
--   size_t window;
--   uint64_t moving_factor;
--   unsigned digits;
--   char otp[10];
--   time_t now, when, t0, time_step_size;
--   int totpflags = 0;
--
--   set_program_name (argv[0]);
--
--   if (cmdline_parser (argc, argv, &args_info) != 0)
--     return EXIT_FAILURE;
--
--   if (args_info.version_given)
--     {
--       char *p;
--       int l = -1;
--
--       if (strcmp (oath_check_version (NULL), OATH_VERSION) != 0)
-- 	l = asprintf (&p, "OATH Toolkit liboath.so %s oath.h %s",
-- 		      oath_check_version (NULL), OATH_VERSION);
--       else if (strcmp (OATH_VERSION, PACKAGE_VERSION) != 0)
-- 	l = asprintf (&p, "OATH Toolkit liboath.so/oath.h %s", OATH_VERSION);
--       version_etc (stdout, "oathtool", l == -1 ? "OATH Toolkit" : p,
-- 		   PACKAGE_VERSION, "Simon Josefsson", (char *) NULL);
--       if (l != -1)
-- 	free (p);
--       return EXIT_SUCCESS;
--     }
--
--   if (args_info.help_given)
--     usage (EXIT_SUCCESS);
--
--   if (args_info.inputs_num == 0)
--     {
--       cmdline_parser_print_help ();
--       emit_bug_reporting_address ();
--       return EXIT_SUCCESS;
--     }
--
--   rc = oath_init ();
--   if (rc != OATH_OK)
--     error (EXIT_FAILURE, 0, "liboath initialization failed: %s",
-- 	   oath_strerror (rc));
--
--   if (args_info.base32_flag)
--     {
--       rc = oath_base32_decode (args_info.inputs[0],
-- 			       strlen (args_info.inputs[0]),
-- 			       &secret, &secretlen);
--       if (rc != OATH_OK)
-- 	error (EXIT_FAILURE, 0, "base32 decoding failed: %s",
-- 	       oath_strerror (rc));
--     }
--   else
--     {
--       secretlen = 1 + strlen (args_info.inputs[0]) / 2;
--       secret = malloc (secretlen);
--       if (!secret)
-- 	error (EXIT_FAILURE, errno, "malloc");
--
--       rc = oath_hex2bin (args_info.inputs[0], secret, &secretlen);
--       if (rc != OATH_OK)
-- 	error (EXIT_FAILURE, 0, "hex decoding of secret key failed");
--     }
--
--   if (args_info.counter_orig)
--     moving_factor = args_info.counter_arg;
--   else
--     moving_factor = 0;
--
--   if (args_info.digits_orig)
--     digits = args_info.digits_arg;
--   else
--     digits = 6;
--
--   if (args_info.window_orig)
--     window = args_info.window_arg;
--   else
--     window = 0;
--
--   if (digits != 6 && digits != 7 && digits != 8)
--     error (EXIT_FAILURE, 0, "only digits 6, 7 and 8 are supported");
--
--   if (validate_otp_p (args_info.inputs_num) && !args_info.digits_orig)
--     digits = strlen (args_info.inputs[1]);
--   else if (validate_otp_p (args_info.inputs_num) && args_info.digits_orig &&
-- 	   args_info.digits_arg != strlen (args_info.inputs[1]))
--     error (EXIT_FAILURE, 0,
-- 	   "given one-time password has bad length %d != %ld",
-- 	   args_info.digits_arg, strlen (args_info.inputs[1]));
--
--   if (args_info.inputs_num > 2)
--     error (EXIT_FAILURE, 0, "too many parameters");
--
--   if (args_info.verbose_flag)
--     {
--       char *tmp;
--
--       tmp = malloc (2 * secretlen + 1);
--       if (!tmp)
-- 	error (EXIT_FAILURE, errno, "malloc");
--
--       oath_bin2hex (secret, secretlen, tmp);
--
--       printf ("Hex secret: %s\n", tmp);
--       free (tmp);
--
--       rc = oath_base32_encode (secret, secretlen, &tmp, NULL);
--       if (rc != OATH_OK)
-- 	error (EXIT_FAILURE, 0, "base32 encoding failed: %s",
-- 	       oath_strerror (rc));
--
--       printf ("Base32 secret: %s\n", tmp);
--       free (tmp);
--
--       if (args_info.inputs_num == 2)
-- 	printf ("OTP: %s\n", args_info.inputs[1]);
--       printf ("Digits: %d\n", digits);
--       printf ("Window size: %ld\n", window);
--     }
--
--   if (args_info.totp_given)
--     {
--       now = time (NULL);
--       when = parse_time (args_info.now_arg, now);
--       t0 = parse_time (args_info.start_time_arg, now);
--       time_step_size = parse_duration (args_info.time_step_size_arg);
--
--       if (when == BAD_TIME)
-- 	error (EXIT_FAILURE, 0, "cannot parse time `%s'", args_info.now_arg);
--
--       if (t0 == BAD_TIME)
-- 	error (EXIT_FAILURE, 0, "cannot parse time `%s'",
-- 	       args_info.start_time_arg);
--
--       if (time_step_size == BAD_TIME)
-- 	error (EXIT_FAILURE, 0, "cannot parse time `%s'",
-- 	       args_info.time_step_size_arg);
--
--       if (strcmp (args_info.totp_arg, "sha256") == 0)
-- 	totpflags = OATH_TOTP_HMAC_SHA256;
--       else if (strcmp (args_info.totp_arg, "sha512") == 0)
-- 	totpflags = OATH_TOTP_HMAC_SHA512;
--
--       if (args_info.verbose_flag)
-- 	verbose_totp (t0, time_step_size, when);
--     }
--   else
--     {
--       if (args_info.verbose_flag)
-- 	verbose_hotp (moving_factor);
--     }
--
--   if (generate_otp_p (args_info.inputs_num) && !args_info.totp_given)
--     {
--       size_t iter = 0;
--
--       do
-- 	{
-- 	  rc = oath_hotp_generate (secret,
-- 				   secretlen,
-- 				   moving_factor + iter,
-- 				   digits,
-- 				   false, OATH_HOTP_DYNAMIC_TRUNCATION, otp);
-- 	  if (rc != OATH_OK)
-- 	    error (EXIT_FAILURE, 0,
-- 		   "generating one-time password failed (%d)", rc);
--
-- 	  printf ("%s\n", otp);
-- 	}
--       while (window - iter++ > 0);
--     }
--   else if (generate_otp_p (args_info.inputs_num) && args_info.totp_given)
--     {
--       size_t iter = 0;
--
--       do
-- 	{
-- 	  rc = oath_totp_generate2 (secret,
-- 				    secretlen,
-- 				    when + iter * time_step_size,
-- 				    time_step_size, t0, digits, totpflags,
-- 				    otp);
-- 	  if (rc != OATH_OK)
-- 	    error (EXIT_FAILURE, 0,
-- 		   "generating one-time password failed (%d)", rc);
--
-- 	  printf ("%s\n", otp);
-- 	}
--       while (window - iter++ > 0);
--     }
--   else if (validate_otp_p (args_info.inputs_num) && !args_info.totp_given)
--     {
--       rc = oath_hotp_validate (secret,
-- 			       secretlen,
-- 			       moving_factor, window, args_info.inputs[1]);
--       if (rc == OATH_INVALID_OTP)
-- 	error (EXIT_OTP_INVALID, 0,
-- 	       "password \"%s\" not found in range %ld .. %ld",
-- 	       args_info.inputs[1],
-- 	       (long) moving_factor, (long) moving_factor + window);
--       else if (rc < 0)
-- 	error (EXIT_FAILURE, 0,
-- 	       "validating one-time password failed (%d)", rc);
--       printf ("%d\n", rc);
--     }
--   else if (validate_otp_p (args_info.inputs_num) && args_info.totp_given)
--     {
--       rc = oath_totp_validate4 (secret,
-- 				secretlen,
-- 				when,
-- 				time_step_size,
-- 				t0,
-- 				window,
-- 				NULL, NULL, totpflags, args_info.inputs[1]);
--       if (rc == OATH_INVALID_OTP)
-- 	error (EXIT_OTP_INVALID, 0,
-- 	       "password \"%s\" not found in range %ld .. %ld",
-- 	       args_info.inputs[1],
-- 	       (long) ((when - t0) / time_step_size - window / 2),
-- 	       (long) ((when - t0) / time_step_size + window / 2));
--       else if (rc < 0)
-- 	error (EXIT_FAILURE, 0,
-- 	       "validating one-time password failed (%d)", rc);
--       printf ("%d\n", rc);
--     }
--
--   free (secret);
--   oath_done ();
--
--   return EXIT_SUCCESS;
-- }
