{-#LANGUAGE FlexibleContexts #-}

import Control.Monad.IO.Class
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.Environment
import System.FilePath
import System.Process
import Graphics.X11
import Graphics.X11.Xinerama
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.MouseResize
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
import XMonad.Layout.Fullscreen
import XMonad.Layout.GridVariants
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
-- import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.Replace
import XMonad.Util.XSelection
import System.Exit
import XMonadLocal

myLayoutHook res = avoidStruts $ mouseResize $ windowArrange $ toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    spacing          = if res == Res2160 then 15 else 5
    mySpacing  i     = spacingRaw False (Border i i i i) True (Border i i i i) True
    mySpacing' i     = spacingRaw True  (Border i i i i) True (Border i i i i) True
    mySpacing''      = mySpacing' spacing
    myTabTheme       = def
    border x         = smartBorders $ withBorder 2 x
    subBorder        = smartBorders
    layoutdefaults n = renamed [Replace n] . border . windowNavigation . addTabs shrinkText myTabTheme . subLayout [] (subBorder Simplest) . limitWindows 12 . mySpacing''
    myDefaultLayout  = tall ||| tabs ||| grid ||| threeCol ||| threeRow -- ||| myMagnify ||| monocle ||| floats ||| spirals

    tall      = layoutdefaults "tall"       $ ResizableTall 1 (3/100) (1/2) []
    grid      = layoutdefaults "grid"       $ mkToggle (single MIRROR) $ Grid (16/10)
    threeCol  = layoutdefaults "threeCol"   $ ThreeCol 1 (3/100) (1/2)
    threeRow  = layoutdefaults "threeRow"   $ Mirror $ ThreeCol 1 (3/100) (1/2)
    myMagnify = layoutdefaults "magnify"    $ magnifier $ ResizableTall 1 (3/100) (1/2) []
    spirals   = layoutdefaults "spirals"    $ spiral (6/7)

    tabs      = renamed [Replace "tabs"]    $ noBorders $ tabbed shrinkText myTabTheme
    monocle   = renamed [Replace "monocle"] $ noBorders $ windowNavigation $ addTabs shrinkText myTabTheme $ subLayout [] (subBorder Simplest) $ limitWindows 20 Full
    floats    = renamed [Replace "floats"]  $ border $ limitWindows 20 simplestFloat

colourNormal  = "#0087af"
colourUnused  = "#5f5f5f"
colourCurrent = "#00afd7"
colourUrgent  = "#ff6c6b"

colourBorder  = "#404040"
colourFocus   = "#208020"

-- windowCount :: X (Maybe String)
-- windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
--     where i = fromJust $ M.lookup ws myWorkspaceIndices

centrePlacement = (placeHook (withGaps (16,0,16,0) (fixed (0.5,0.5))) <>)
smartCPlacement = (placeHook (withGaps (16,0,16,0) (smart (0.5,0.5))) <>)

data Res = Res2160 | Res1080 | ResOther deriving (Show, Eq, Read, Ord, Bounded, Enum)
getRes :: IO Res
getRes = do
  screenheights <- map rect_height <$> (openDisplay [] >>= getScreenInfo)
  return $ case take 1 screenheights of
    [2160] -> Res2160
    [1080] -> Res1080
    _      -> ResOther

myworkspaces :: Res -> [String]
myworkspaces Res2160 = ["one", "web", "three", "four", "five", "matrix", "seven", "eight", "kodi"]
myworkspaces _       = map show [1..8]

main :: IO ()
main = do
    res <- getRes
    safeSpawn "xset"  ["s", "off"]
    safeSpawn "xset"  ["-dpms"]
    safeSpawn "picom" ["-b", "--backend", "glx"]
    safeSpawn "mocp"  ["-S", "-m", "/media/Audio"]
    homeDir <- getEnv "HOME"
    safeSpawn "sh" [(homeDir </> ".xsession")]
    let myworkspaces' = myworkspaces res
--     safeSpawnProg $ homeDir ++ "/.local/bin/tray"
--     safeSpawnProg "volumeicon"
--     safeSpawn "solaar" ["-w", "hide"]
--     xmproc <- spawnPipe $ "xmobar"
    let sb = statusBarProp "xmobar" (clickablePP xmobarPP {
      ppTitle           = shorten 50 . xmobarStrip
    , ppCurrent         = xmobarColor colourCurrent "" . xmobarBorder "Bottom mb=2" colourCurrent 1
    , ppVisible         = xmobarColor colourNormal ""
    , ppHidden          = xmobarColor colourNormal ""
    , ppHiddenNoWindows = xmobarColor colourUnused ""
    , ppLayout          = xmobarAction "xdotool key super+space" "1"
    , ppSep             = " | "
    , ppUrgent          = xmobarColor colourUrgent "" . wrap "!" "!"
    })
    safeSpawnProg $ homeDir </> ".xplanet/xplanet.sh"
    replace
    xmonad . withSB sb $ gnomeConfig
        { workspaces = myworkspaces'
        , terminal = homeDir </> ".local/bin/kitty"
        , manageHook = composeAll
          [ manageDocks
          , fullscreenManageHook
          , isFullscreen --> doFullFloat
          , className =? "Qalculate" --> centrePlacement doFloat
          , className =? "Solaar" --> centrePlacement doFloat
          , className =? "Nm-connection-editor" --> centrePlacement doFloat
          , className =? "Virt-manager" --> centrePlacement doFloat
          , className =? "Gimp" --> doFloat
          , className =? "MPlayer" --> doFullFloat
          , className =? "Totem" --> doFullFloat
          , className =? "Brave-browser" --> doShift (myworkspaces' !! 1)
          , className =? "Kodi" --> doShift (last myworkspaces')
          , className =? "nheko" --> doShift (myworkspaces' !! 5)
          , manageHook def
          ]
        , layoutHook = myLayoutHook res
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , normalBorderColor  = colourBorder
        , focusedBorderColor = colourFocus
        , keys = \c@XConfig {modMask = modm} -> M.fromList $ [
            ((0, xF86XK_AudioMute),         safeSpawn     "amixer"        ["set", "Master", "toggle"])
          , ((0, xF86XK_AudioRaiseVolume),  safeSpawn     "amixer"        ["-M", "set", "Master", "on", "10%+"])
          , ((0, xF86XK_AudioLowerVolume),  safeSpawn     "amixer"        ["-M", "set", "Master", "on", "10%-"])
          , ((0, xF86XK_AudioPrev),         safeSpawn     "mocp"          ["-r"])
          , ((0, xF86XK_AudioNext),         safeSpawn     "mocp"          ["-f"])
          , ((0, xF86XK_AudioPlay),         safeSpawn     "mocp"          ["-G"])
          , ((0, xF86XK_MonBrightnessDown), safeSpawn     "brightnessctl" ["s", "10%-"])
          , ((0, xF86XK_MonBrightnessUp),   safeSpawn     "brightnessctl" ["s", "10%+"])
          , ((0, xF86XK_Calculator),        safeSpawnProg "qalculate")
          ] ++ M.toList (keys def c)
        } `additionalKeysP` (
--         [ ("M-S-z", spawn "xscreensaver-command -lock; xset dpms force off")
        [ ("M-S-<Return>", safeSpawnProg $ homeDir </> ".local/bin/kitty")
        , ("M-C-<Return>", safeSpawnProg "x-terminal-emulator")
        , ("M-S-b", safeSpawnProg "x-www-browser")
        , ("M-S-h", safeSpawnProg "nautilus")
        , ("M-S-m", safeSpawnProg "nheko")
        , ("M-C-k", safeSpawnProg "kodi")
        , ("C-<Print>", safeSpawn "sleep" ["0.2"] >> safeSpawn "scrot" ["-s"])
        , ("<Print>", safeSpawnProg "scrot")
        , ("M-C-r", safeSpawn "xmonad" ["--recompile"] >> safeSpawn "xmonad" ["--restart"])
        , ("M-S-r", safeSpawn "xmonad" ["--restart"])
        , ("M-S-q", io exitSuccess)
        , ("M-S-c", kill1)
        , ("M-S-s", safeSpawn "sudo" ["systemctl", "suspend"])
        , ("M-u", safeSpawn "x-terminal-emulator" ["mocp"])
        ] ++ local_shortcuts)
