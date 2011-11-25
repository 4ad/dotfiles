------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad
import XMonad.Actions.GridSelect
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
main = do
  xmonad $ defaultConfig
    { keys = myKeys 
    }
 
myGSConfig = defaultGSConfig
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod1Mask, xK_Tab), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((mod4Mask, xK_Tab), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspac
    ]
