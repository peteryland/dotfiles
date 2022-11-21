import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.Environment
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.MouseResize
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
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
import System.Exit

mySpacing  i = spacingRaw False (Border i i i i) True (Border i i i i) True
mySpacing' i = spacingRaw True  (Border i i i i) True (Border i i i i) True

myTabTheme = def
-- border = id
border x = smartBorders x
-- subBorder = id
subBorder = smartBorders

tall     = renamed [Replace "tall"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ limitWindows 12
           $ mySpacing' 10
           $ ResizableTall 1 (3/100) (1/2) []
myMagnify  = renamed [Replace "magnify"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing' 15
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ border
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ limitWindows 12
           $ mySpacing' 15
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ mySpacing' 15
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ limitWindows 7
           $ mySpacing' 15
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ border
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (subBorder Simplest)
           $ mySpacing' 10
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder 2 tall
--                                  ||| myMagnify
--                                  ||| noBorders monocle
--                                  ||| floats
                                 ||| noBorders tabs
                                 ||| withBorder 2 grid
--                                  ||| spirals
                                 ||| withBorder 2 threeCol
--                                  ||| threeRow

color05 = "#51afef"
color06 = "#c678dd"
color09 = "#5b6268"
color02 = "#ff6c6b"
color16 = "#dfdfdf"

okcolor = "#00afd7"
rocolor = "#0087af"
incolor = "#5f5f5f"
-- rocolor = "#d7d700"

myWorkspaces = [" one ", " two ", " three ", " four "]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

-- windowCount :: X (Maybe String)
-- windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
--     where i = fromJust $ M.lookup ws myWorkspaceIndices

centrePlacement = (placeHook (withGaps (16,0,16,0) (fixed (0.5,0.5))) <>)
smartCPlacement = (placeHook (withGaps (16,0,16,0) (smart (0.5,0.5))) <>)

main :: IO ()
main = do
    safeSpawn "xset" ["s", "off"]
    safeSpawn "xset" ["-dpms"]
    safeSpawn "picom" ["-b"]
    safeSpawn "mocp" ["-S", "-m", "/media/Audio"]
    homeDir <- getEnv "HOME"
--     safeSpawnProg $ homeDir ++ "/.local/bin/tray"
--     safeSpawnProg "volumeicon"
--     safeSpawn "solaar" ["-w", "hide"]
--     xmproc <- spawnPipe $ "xmobar " ++ homeDir ++ "/.xmonad/xmobar.config"
    let sb = statusBarProp ("xmobar " ++ homeDir ++ "/.xmonad/xmobar.config") (clickablePP xmobarPP {
      ppTitle           = shorten 50 . xmobarStrip
    , ppCurrent         = xmobarColor okcolor "" . wrap ("<box type=Bottom width=1 mb=4 color=" ++ okcolor ++ ">") "</box>"
    , ppVisible         = xmobarColor rocolor ""
    , ppHidden          = xmobarColor rocolor ""
    , ppHiddenNoWindows = xmobarColor incolor ""
    , ppLayout          = wrap "<action=xdotool key super+space>" "</action>"
    , ppSep             = " | "
    , ppUrgent          = xmobarColor color02 "" . wrap "!" "!"
    , ppOrder           = \(ws:l:t:ex) -> [ws,l,t] -- ++ex++[t]
    })
    safeSpawnProg $ homeDir ++ "/.xplanet/xplanet.sh"
    replace
    xmonad . withSB sb $ gnomeConfig
        { terminal = "kitty"
        , manageHook = composeAll [
            manageDocks
          , className =? "Qalculate" --> centrePlacement doFloat
          , className =? "Solaar" --> centrePlacement doFloat
          , manageHook def
          ]
        , layoutHook = myLayoutHook
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , normalBorderColor = "#404040"
        , focusedBorderColor = "#208020"
        , keys = \c@XConfig {modMask = modm} -> M.fromList $ [
            ((0, xF86XK_AudioMute),        safeSpawn     "amixer" ["set", "Master", "toggle"])
          , ((0, xF86XK_AudioRaiseVolume), safeSpawn     "amixer" ["-M", "set", "Master", "on", "10%+"])
          , ((0, xF86XK_AudioLowerVolume), safeSpawn     "amixer" ["-M", "set", "Master", "on", "10%-"])
          , ((0, xF86XK_AudioPrev),        safeSpawn     "mocp"   ["-r"])
          , ((0, xF86XK_AudioNext),        safeSpawn     "mocp"   ["-f"])
          , ((0, xF86XK_AudioPlay),        safeSpawn     "mocp"   ["-G"])
          , ((0, xF86XK_Calculator),       safeSpawnProg "qalculate")
          ] ++ M.toList (keys def c)
        } `additionalKeysP`
--         [ ("M-S-z", spawn "xscreensaver-command -lock; xset dpms force off")
        [ ("M-S-<Enter>", safeSpawnProg "kitty")
        , ("M-S-b", safeSpawnProg "x-www-browser")
        , ("M-S-h", safeSpawnProg "nautilus")
        , ("C-<Print>", safeSpawn "sleep" ["0.2"] >> safeSpawn "scrot" ["-s"])
        , ("<Print>", safeSpawnProg "scrot")
        , ("M-C-r", safeSpawn "xmonad" ["--recompile"] >> safeSpawn "xmonad" ["--restart"])
        , ("M-S-r", safeSpawn "xmonad" ["--restart"])
        , ("M-S-q", io exitSuccess)
        , ("M-S-c", kill1)
        , ("M-u", safeSpawn "x-terminal-emulator" ["mocp"])
        ]
