{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


--- Inspiration https://github.com/ruhatch/.dotfiles/blob/master/.xmonad/xmonad.hs

import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare
import Control.Monad
import Data.Bool  (bool)
import XMonad.Util.EZConfig
import XMonad.Actions.WindowGo
import XMonad.Actions.GroupNavigation


--- Util functions
findWindows :: String -> X [Window]
findWindows name = do
  withWindowSet $ (\ws -> do
    forM (W.allWindows ws)
      (\w -> do
            s <- withDisplay $ \d -> fmap resClass . liftIO $ getClassHint d w
            return $ bool [] [w] (s == name) :: X [Window]
      ) >>= return . join
    )

--- 

myTerminal :: String
myTerminal = "termite"

---------------- Layout

--chillMulti = avoidStruts (spacing (Tall nmaster rincrement mratio))
  --where 
    --spacing = spacingRaw True (Border 40 40 20 20) True (Border 8 8 40 40) True
    --nmaster = 2
    --rincrement = 3 / 100
    --mratio = 2 / 3

defaultMultiWindow = avoidStruts (spacing (Tall nmaster rincrement mratio))
  where
    spacing = spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True
    nmaster = 1
    rincrement = 3 / 100
    mratio = 1 / 2



myLayout = defaultMultiWindow ||| fullscreenFull Full



-------------- Colors and borders

myNormalBorderColor :: String
myNormalBorderColor = "#FAFAFA"

myFocusedBorderColor :: String
myFocusedBorderColor = myNormalBorderColor

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#Af745f"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 0


myWorkspaces :: [String]
myWorkspaces = workspaceLabels
 where
  workspaceLabels = icons
  --workspaceLabels = zipWith makeLabel [1 .. 10 :: Int] icons
  --makeLabel index icon = (show index) ++ " " ++ icon
  icons =
      [ "∞ woo"
    , "☃ random"
    , "♡ ide"
    , "✉ mail"
    , "★ notes"
    , "☸ chat"
    , "∇ remote"
    , "☄ terminal"
    , "☢ spotify"
    , ""
    ]

------------------------- Startup

myStartupHook :: X ()
myStartupHook =
  spawn
      "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17"
    <+> setDefaultCursor xC_left_ptr
    <+> spawn "hsetroot -solid '#D6D6D6'"
    <+> spawn "xsetroot -cursor_name left_ptr"
    <+> spawn "albert"
    <+> setWMName "LG3D"


--------------------- Loghook

myLogHook :: Handle -> X ()
myLogHook xmproc = 
  fadeInactiveLogHook 0.8 <+>
  (dynamicLogWithPP $ xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = const ""
    , ppLayout  = const ""
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep     = "   "
    })
    <+> historyHook



---------------------- Run xmonad

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . docks . E.ewmh $ defaults { logHook = myLogHook xmproc } `additionalKeysP` [ 
          ("M-m", do
                    windows $ W.view "✉ mail"
                    runOrRaise "mailspring" (className=? "Mailspring"))
        , ("M-n", do 
                    windows $ W.view "★ notes")
        , ("M-w", windows $ W.view "∞ home")
        , ("M-p", spawn "sh /home/jonas/.config/lighthouse/lighthouse.sh")
        , ("M-q", nextMatch History (return True))
        , ("M-c", do
                 windows $ W.view "☸ scratchpad")

        , ("M-e", windows $ W.view "♡ ide")
        , ("M-s", do windows $ W.view "☢ spotify")
        , ("M-t", windows $ W.view "☄ terminal")
        , ("M-r", windows $ W.view "∇ remote")
        , ("M-<F10>", spawn "shutter")
        , ("M-a s", spawn "systemctl --user start friday-voice-assistant.service")
        , ("M-a q", spawn "systemctl --user stop friday-voice-assistant.service")
      ]

--------------------- Combine

defaults = def
  {
    -- simple stuff
    terminal      = myTerminal
  , borderWidth   = myBorderWidth
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , manageHook = manageDocks <+> manageHook defaultConfig

    -- key bindings
    -- hooks, layouts
  , workspaces    = myWorkspaces
  , layoutHook    = myLayout
  , startupHook   = myStartupHook
  , handleEventHook = E.fullscreenEventHook --handleEventHook defaultConfig <+> docksEventHook
  }
