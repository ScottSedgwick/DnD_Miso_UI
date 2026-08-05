-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Components.Main ( app )

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
-----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif
