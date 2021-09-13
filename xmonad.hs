{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}



import           Data.Bool                         (bool)
import           Data.Maybe
import           Data.Monoid

import qualified Data.Map                          as M
import qualified XMonad.Hooks.EwmhDesktops         as E
import qualified XMonad.StackSet                   as W

import           Graphics.X11.ExtraTypes

import           System.IO

import           XMonad.Actions.FloatKeys

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WindowGo

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.Mosaic
import           XMonad.Layout.Spacing
import           XMonad.Layout.Grid

import           XMonad.Util.Cursor
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run                   (runProcessWithInput,
                                                    spawnPipe)
import           XMonad.Util.WorkspaceCompare

import           Control.Monad

import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.XMonad

import           Foreign.C.String
import           XMonad.Util.XUtils

---------------- Layout

--chillMulti = avoidStruts (spacing (Tall nmaster rincrement mratio))
  --where
    --spacing = spacingRaw True (Border 40 40 20 20) True (Border 8 8 40 40) True
    --nmaster = 2
    --rincrement = 3 / 100
    --mratio = 2 / 3

-- Origional layout that I had.. was pretty ok - testing mosaic now
--defaultMultiWindow = avoidStruts (spacing (Tall nmaster rincrement mratio))
  --where
    --spacing = spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True
    --nmaster = 1
    --rincrement = 3 / 100
    --mratio = 1 / 2


defaultGridWindow = avoidStruts (spacing Grid)
  where
    spacing = spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True




myLayout = defaultGridWindow ||| fullscreenFull Full



myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
 --where
  --workspaceLabels = icons
  --icons =
      --[ "∞"
    --, "☃"
    --, "♡"
    --, "✉"
    --, "★"
    --, "☸"
    --, "∇"
    --, "☄"
    --, "☢"
    --, ""
    --]


------------------------- Startup


myStartupHook :: X ()
myStartupHook =
  spawn "hsetroot -solid '#D6D6D6'"
    <+> spawn "xsetroot -cursor_name left_ptr"
    <+> setDefaultCursor xC_left_ptr
    <+> setWMName "LG3D"
    <+> spawn "feh --bg-scale $HOME/dotfiles/coolbg.jpg"
    <+> spawn "exec $HOME/dotfiles/start_eww"


------------------------- My Prompt
myPromptBackgroundColor = "#080a10"
myPromptBackgroundHColor = myPromptBackgroundColor

myPromptContentColor = "#38bf71"
myPromptContentHColor = "#CE8CE9"

myPromptFont = "xft:VictorMono Nerd Font:regular:pixelsize=15"


myXPromptConfig :: XPConfig
myXPromptConfig =
  XPC
    { promptBorderWidth = 0
    , alwaysHighlight = True
    , height = 35
    , historySize = 128
    , font = myPromptFont
    , bgColor = myPromptBackgroundColor
    , fgColor = myPromptContentColor
    , bgHLight = myPromptBackgroundHColor
    , fgHLight = myPromptContentHColor

    -- This entry has to be here but bw is 0 so cant be seen
    , borderColor = "#000000"
    -- First is Y position where 0 is top 1 is bottom
    -- Second is X position where 1 is left-most 0 is rightmost
    , position = CenteredAt 0.3 0.5
    , autoComplete = Just (100000)
    , showCompletionOnTab = False
    , maxComplRows = Just 10
    , promptKeymap = defaultXPKeymap
    , completionKey = (0, xK_Tab)
    , changeModeKey = xK_grave
    , historyFilter = id
    , defaultText = []
    }


data MyPrompt = MyPrompt String (M.Map String Window)


instance XPrompt MyPrompt where
  showXPrompt (MyPrompt name _) = name
  nextCompletion = getNextOfLastWord
  commandToComplete (MyPrompt name _) command = command
  completionToCommand (MyPrompt name _) completion = completion
  completionFunction  (MyPrompt name w) entry =
    do
      python <- pythonEval
      return $ windowCompletions ++ appCompletions ++ python
    where
      applications = [ "spotify"
                      , "firefox"
                      , "inkscape"
                      , "etcher"
                      , "transmission"
                      , "google-chrome"
                      , "google-mail-jonasrsv"
                      , "google-calendar-jonasrsv"
                      , "mail"
                      , "mail-bridge"
                      , "bridge"
                      , "calendar"
                      , "power"
                      , "volume"
                      , "sound"
                      , "discord"
                      , "wifi"
                      , "ipy"
                      , "1password"
                      , "password"
                      , "chrome"
                      ]


      -- Use this with autoComplete = Just x
      matchPredicate a b = take (length a) b == a

      -- Use this to get fuzzy compl (with autoComplete = Nothing) in XPConfig
      --let matchPredicate = fuzzyMatch

      -- To use with immediate completion
      appCompletions = filter (\app -> matchPredicate entry app) applications
      windowCompletions = filter (\win -> matchPredicate entry win) . map fst . M.toList $ w
      --bringCompletions = filter (\win -> matchPredicate entry ("b"++win)) . map fst . M.toList $ w

      -- This runs a python interpreter on the raw input and returns the result
      -- I made a tiny python script called calc that reads from stdin - uses eval(..) - and prints res
      pythonEval = do
        res <- runProcessWithInput "calc" [] entry
        case res of
          ""        -> return []
          something -> return ["Python > " ++ something, ""]




  modeAction (MyPrompt _ w) completion _ = case completion of
    "spotify" -> spawn "spotify"
    "firefox" -> spawn "firefox"
    "inkscape" -> spawn "inkscape"
    "etcher" -> spawn "etcher"
    "transmission" -> spawn "transmission"
    "google-chrome" -> spawn "google-chrome"
    "chrome" -> spawn "google-chrome"
    "power" -> spawn "xfce4-power-manager-settings"
    "volume" -> spawn "pavucontrol"
    "sound" -> spawn "pavucontrol"
    "discord" -> spawn "discord"
    "wifi" -> spawn "termite -e nmtui"
    "ipy" -> spawn "termite -e ipython"
    "1password" -> spawn "1password"
    "password" -> spawn "1password"
    "google-mail-jonasrsv" -> spawn "firefox https://mail.google.com/mail/u/0/#inbox"
    "google-calendar-jonasrsv" -> spawn "firefox https://calendar.google.com/calendar/u/0/r"
    "calendar" -> spawn "firefox https://calendar.google.com/calendar/u/0/r"
    "mail" -> spawn "termite -e neomutt"
    "mail-bridge" -> spawn "termite -e protonmail-bridge"
    "bridge" -> spawn "termite -e protonmail-bridge"
    _ -> case M.lookup completion w of
      Just window -> (windows . W.focusWindow) $ window
      Nothing     -> return ()


myPrompt :: XPConfig -> X (M.Map String Window) -> X ()
myPrompt promptConf winmap =
  do
    prompt <- evalMyPrompt

    mkXPrompt prompt promptConf (completionFunction prompt) (\compl -> modeAction prompt compl "")
  where
    evalMyPrompt = do
      wm <- winmap
      return $ MyPrompt "λ  " wm




--------------------- Loghook

myXmobarPP :: Handle -> [(WorkspaceId, [String])] -> PP
myXmobarPP xmproc wsMap = xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = const ""
    , ppLayout  = const ""
    , ppCurrent = \workspace -> (xmobarColor "#CE8CE9" "" $ displayName workspace)
    , ppVisible = \workspace -> "[ " ++ (displayName workspace) ++ " ]"
    , ppVisibleNoWindows = Nothing
    , ppHidden =  \workspace -> (displayName workspace)
    , ppUrgent = \workspace -> (xmobarColor "#ff0000" "" $ displayName workspace)
    , ppWsSep     = "  |  "
    }
  where
    displayName workspace = case lookup workspace wsMap of
      -- TODO maybe better window name choosing here?
      Just windowNames -> case windowNames of
            []    -> "unknown"
            names -> ((workspace ++ " ") ++) . shortenName . head $ names
      Nothing -> "unknown"

    shortenName name = case length name > 10 of
      True  -> (take 10 name) ++ "..."
      False -> name

-- Functions for getting window names given a workspace ID

workspacesGrouped :: X [(WorkspaceId, [String])]
workspacesGrouped = do
  ws <- gets windowset
  let x = map (W.workspace) (W.current ws : W.visible ws)
  let y = (W.hidden ws)
  sequence $ fmap (\v -> fmap ((,) $ W.tag v) (getWorkspaceWindowTitles v)) $ x ++ y


getWorkspaceWindowTitles :: W.Workspace i l Window -> X [String]
getWorkspaceWindowTitles w = do
  withDisplay $ \d ->
    (liftIO $ forM
      (W.integrate' $ W.stack w)
      (\z -> getWindowTitle z d)
    )

getWindowTitle :: Window -> Display -> IO String
getWindowTitle w d = getTextProperty d w wM_NAME >>= (peekCString . tp_value)

-- This is where we put the 'fadeInactiveLogHook' if we have a powerful computer

myXmobarLogger :: Handle -> X()
myXmobarLogger xmproc = do
  wsMap <- workspacesGrouped
  dynamicLogWithPP (myXmobarPP xmproc wsMap)

myLogHook :: Handle -> Handle -> X ()
myLogHook xmprocw1 xmprocw2 =
        myXmobarLogger xmprocw1
    <+> myXmobarLogger xmprocw2
    <+> historyHook

------------------------- Event Hook, new screens or something happends
myHandleEventHook :: Event -> X Data.Monoid.All
myHandleEventHook = E.fullscreenEventHook


---------------------- Run xmonad

main :: IO ()
main = do

  --Always try to spawn two windows, for multiscreen, should be some better solution but .. meh
  xmprocw1 <- spawnPipe "xmobar -x 0"
  xmprocw2 <- spawnPipe "xmobar -x 1"

  xmonad . docks . E.ewmh $ defaults { logHook = myLogHook xmprocw1 xmprocw2 }



defaults = def
  {
    -- simple stuff
    terminal      = "alacritty"
  , borderWidth   = 1 -- 1px
  --, focusedBorderColor = "#CE8CE9"
  , focusedBorderColor = "#000000"
  , normalBorderColor = "#000000"
  , manageHook = manageDocks <+> manageHook defaultConfig

    -- key bindings
    -- hooks, layouts
  , workspaces    = myWorkspaces
  , layoutHook    = myLayout
  , startupHook   = myStartupHook
  , handleEventHook = myHandleEventHook --handleEventHook defaultConfig <+> docksEventHook
  , keys          = \c -> mykeys c `M.union` keys defaultConfig c
  }
  where

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                                then W.sink w s
                                else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    mykeys (XConfig {modMask = modm}) = M.fromList $
         [   ((modm, xK_x), spawn "spotify")



           --       Workspace Related functions
           -- Open new workspace
           , ((modm, xK_n), moveTo Next EmptyWS)
           -- Send current window to empty workspace
           , ((modm .|. shiftMask, xK_n), withFocused $ (\window -> do
                    sendToEmptyWorkspace  -- Send current window to an empty workspace
                    (windows . W.focusWindow) $ window)) -- Refocus the current window in the new workspace

           -- Toggling
           , ((modm, xK_p), nextMatch History (return True))
           , ((modm .|. shiftMask, xK_p), toggleWS)

           , ((modm, xK_F10), spawn "shutter")
           , ((modm, xK_F9), spawn "systemctl suspend")

           -- Change Keyboard Layout
           , ((modm, xK_s), submap . M.fromList $ [
                    ((0, xK_e), spawn "setxkbmap -layout us")
                  , ((0, xK_s), spawn "setxkbmap -layout se")
           ])
           -- Start Prompt
           , ((modm, xK_o), myPrompt myXPromptConfig windowMap)
           -- Open oryx configurator
           , ((modm .|. shiftMask, xK_m), spawn "google-chrome https://configure.ergodox-ez.com/moonlander/layouts/WWQzn/latest/0")
           

           -- Controlling floating windows
           , ((modm, xK_u), withFocused toggleFloat)
           , ((modm, xK_Up), withFocused $ keysMoveWindow (0, -20))
           , ((modm, xK_Down), withFocused $ keysMoveWindow (0, 20))
           , ((modm, xK_Right), withFocused $ keysMoveWindow (20, 0))
           , ((modm, xK_Left), withFocused $ keysMoveWindow (-20, 0))
           , ((modm .|. shiftMask, xK_Up), withFocused $ keysResizeWindow (0, -20) (0, 0))
           , ((modm .|. shiftMask, xK_Down), withFocused $ keysResizeWindow (0, 20) (0, 0))
           , ((modm .|. shiftMask, xK_Right), withFocused $ keysResizeWindow (20, 0) (0, 0))
           , ((modm .|. shiftMask, xK_Left), withFocused $ keysResizeWindow (-20, 0) (0, 0))
         ]

