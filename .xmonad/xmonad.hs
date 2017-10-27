import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName

import Data.Char (toUpper)
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Map as M

myWorkspaces = ["1","2","3","4","5","6"]
 
main = xmonad kde4Config
    { startupHook        = startupHook kde4Config <> setWMName "LG3D",
      modMask            = mod1Mask
    , manageHook         = manageHook kde4Config <+> myManageHook
    , focusFollowsMouse  = True
    , workspaces         = myWorkspaces
    , layoutHook         = smartBorders . avoidStruts $ (Tall 1 (3/100) (1/2) ||| Full)
    , borderWidth        = 1
    , normalBorderColor  = "#444444"
    , focusedBorderColor = "#005577"
    , keys               =  \c -> deleteKeys c $ myKeys c `M.union` keys kde4Config c 
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm,                xK_p),  spawn "dmenu_run")
  , ((modm,                xK_F4), kill) 
  , ((modm .|. shiftMask,  xK_r),  spawn "xmonad --recompile && xmonad --restart")
  , ((modm,                xK_m),  sendMessage NextLayout)
  ]

deleteKeys conf@(XConfig {XMonad.modMask = modm}) keys = foldl (flip M.delete) keys delKeys
  where delKeys = [(modm, xK_w), (modm, xK_e), (modm, xK_r)]

myManageHook = composeAll $ customFloats <> plasmaFloats  
  where customFloats = [ isFullscreen --> doFullFloat
                       , className =? "Keepassx2" --> doFloat
                       , className =? "seafile-applet" --> doFloat
                       , className =? "Seafile Client" --> doFloat
                       ]
        plasmaFloats = [ className =? name --> doFloat | name <- plasmaNames ]
        plasmaNames  = [ "yakuake", "kmix", "plasma", "plasma-desktop", "plasmashell"
                       ,  "krunner", "ksplashsimple", "ksplashqml", "ksplashx" ] 
                     & concatMap (\n -> [ n, toUpper (head n) : (tail n) ])
    
