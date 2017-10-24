module Main where

import LibOath.Internal

main :: IO ()
main = do
    oath_init
    oath_done
    pure ()
