------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
main =
    xmonad $ defaultConfig
        { handleEventHook    = fullscreenEventHook
        } `additionalKeys`
        ( myScrotKeys )
 
myScrotKeys = 
    [ ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s") -- take a screen shot of a window
    , ((0, xK_Print), spawn "scrot") -- take a screen shot of the screen
    ]
