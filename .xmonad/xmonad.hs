import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Hooks.SetWMName
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as SS

import XMonad.Layout.Spacing

import Data.Char (toUpper)
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Map as M

main :: IO ()
main = do
  let config = kde4Config
  xmonad config
    { startupHook        = startupHook config <> setWMName "LG3D"
    , modMask            = mod1Mask
    , terminal           = myTerminal
    , manageHook         = manageHook config <+> myManageHook <+> manageScratchpad
    , focusFollowsMouse  = True
    , workspaces         = myWorkspaces
    , layoutHook         = smartBorders . avoidStruts $ 
                             (spacing 5 (Tall 1 (3/100) (1/2)) ||| Full)
    , borderWidth        = 1
    , normalBorderColor  = "#444444"
    , focusedBorderColor = "#005577"
    , keys               =  \c -> deleteKeys c $ myKeys c `M.union` keys config c
    }

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6"]

myTerminal :: String
myTerminal = "termite"

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm,                 xK_p),   spawn "dmenu_run")
  , ((modm,                 xK_o),   spawn "networkmanager_dmenu")
  , ((modm .|. controlMask, xK_t),   spawn myTerminal)
  , ((modm,                 xK_F4),  kill)
  , ((modm .|. shiftMask,   xK_r),   spawn "xmonad --recompile && xmonad --restart")
  , ((modm,                 xK_m),   sendMessage NextLayout)
  , ((0,                    xK_F10), scratchpadSpawnActionCustom "termite --name=scratchpad")
  --  , ((modm .|. controlMask, xK_l),   spawn "i3lock -efc 414141")
  ]

deleteKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()) -> M.Map (ButtonMask, KeySym) (X ())
deleteKeys conf@(XConfig {XMonad.modMask = modm}) keys = foldl (flip M.delete) keys delKeys
  where delKeys = [(modm, xK_w), (modm, xK_e), (modm, xK_r)]

myManageHook :: ManageHook
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

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook $ SS.RationalRect left top width height
  where left   = 0.1
        top    = 0.15
        width  = 0.8
        height = 0.7
