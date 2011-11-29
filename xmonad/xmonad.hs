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
 
-- import qualified XMonad.StackSet as W
-- import qualified Data.Map as M
 
main = do
    xmonad $ defaultConfig
        { handleEventHook    = fullscreenEventHook
--        , modMask            = myModMask
--        , keys               = myKeys
        } `additionalKeys`
        [ ((mod1Mask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
 
-- myModMask = mod4Mask -- Windows key
-- myGSConfig = defaultGSConfig

-- myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
--     [ ((mod1Mask, xK_Tab), goToSelected myGSConfig) -- display grid select and go to selected window
--     , ((mod4Mask, xK_Tab), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspac
--     ]
