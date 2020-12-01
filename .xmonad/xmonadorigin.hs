import XMonad hiding ((|||))
import qualified XMonad.StackSet as W
import qualified Data.Map -- as M
import XMonad.Config.Azerty
import Graphics.X11.ExtraTypes.XF86

-- Useful for rofi
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP, additionalMouseBindings)
import System.IO
import System.Exit
import XMonad.Util.SpawnOnce
-- Last window
import XMonad.Actions.GroupNavigation
import XMonad.Actions.WindowGo (runOrRaise)
-- Last workspace. Seems to conflict with the last window hook though so just
-- disabled it.
-- import XMonad.Actions.CycleWS
-- import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Layout.Tabbed
import XMonad.Hooks.InsertPosition
import XMonad.Layout.SimpleDecoration (shrinkText)
-- Imitate dynamicLogXinerama layout
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
-- Order screens by physical location
import XMonad.Actions.PhysicalScreens
   -- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import Data.Default
-- For getSortByXineramaPhysicalRule
import XMonad.Layout.LayoutCombinators
-- smartBorders and noBorders
import XMonad.Layout.NoBorders
-- spacing between tiles
import XMonad.Layout.Spacing
-- Insert new tabs to the right: https://stackoverflow.com/questions/50666868/how-to-modify-order-of-tabbed-windows-in-xmonad?rq=1
-- import XMonad.Hooks.InsertPosition
import XMonad.Actions.Search as S

--- Layouts
-- Resizable tile layout
import XMonad.Layout.ResizableTile
-- Simple two pane layout.
import XMonad.Layout.TwoPane
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Dwindle

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

myFont :: String
myFont = "xft:Hack Nerd Font:bold:size=9:antialias=true:hinting=true"

myWorkspaces :: [(String, String)]
myWorkspaces = [
  ( "Term"     ,"`" ) ,
  ( "Web"      ,"1" ) ,
  ( "Discord"  ,"2" ) ,
  ( "brp"      ,"3" ) ,
  ( "azure"    ,"4" ) ,
  ( "search"   ,"5" ) ,
  ( "6"        ,"6" ) ,
  ( "code"     ,"7" ) ,
  ( "8"        ,"8" ) ,
  ( "9"        ,"9" ) ,
  ( "0"        ,"0" ) ,
  ( "scrib"    ,"," ) ,
  ( "notes"    ,"." )

  ]


myTabConfig = def { activeColor = "#556064"
                  , inactiveColor = "#2F3D44"
                  , urgentColor = "#FDF6E3"
                  , activeBorderColor = "#454948"
                  , inactiveBorderColor = "#454948"
                  , urgentBorderColor = "#268BD2"
                  , activeTextColor = "#fea44c"
                  , inactiveTextColor = "#ff8a18"
                  , urgentTextColor = "#ff8a18"
                  , fontName = myFont
                  }

myStartupHook = do
          spawnOnce "xmobar &"
          spawnOnce "nitrogen --restore &"
          spawnOnce "volumeicon &"
          spawnOnce "picom &"
          spawnOnce "discord"
          spawnOnce "fluxgui &"
          spawnOnce "greenclip daemon &"
          setWMName "LG3D"

myLayout = avoidStruts $
  -- noBorders (tabbed shrinkText myTabConfig)
  tiled
  ||| Mirror tiled
  ||| noBorders Full
  -- ||| twopane
  -- ||| Mirror twopane
  -- ||| emptyBSP
  -- ||| Spiral L XMonad.Layout.Dwindle.CW (3/2) (11/10) -- L means the non-main windows are put to the left.

  where
     -- The last parameter is fraction to multiply the slave window heights
     -- with. Useless here.
     tiled = spacing 3 $ ResizableTall nmaster delta ratio []
     -- In this layout the second pane will only show the focused window.
     twopane = spacing 3 $ TwoPane delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myPP = def { ppCurrent = xmobarColor "#ff8a18" "" . wrap "[" "]"
           , ppTitle = xmobarColor "#ff8a18" "" . shorten 60
           , ppVisible = wrap "(" ")"
           , ppUrgent  = xmobarColor "red" "yellow"
           , ppSort = getSortByXineramaPhysicalRule def
           }

myManageHook = composeAll 
  [ -- isFullscreen --> doFullFloat
  className =? "discord" --> doShift "Discord"
  ]
myKeys conf@(XConfig {XMonad.modMask = modm}) = Data.Map.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- XF86AudioMute = 0x1008ff12
    , ((0, xF86XK_AudioMute), spawn "amixer -q set Master,0 toggle")
    -- XF86AudioRaiseVolume = 0x1008ff13
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master,0 5%+")
    -- XF86AudioLowerVolume = 0x1008ff11
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master,0 5%-")
    -- Ctrl+XF86AudioMute=microphone mute (till i figure out how to use dedicated hw button)
    , ((controlMask, xF86XK_AudioMute), spawn "amixer -q set Capture,0 toggle")
    -- Ctrl+XF86AudioRaiseVolume=microphone raise volume
    , ((controlMask, xF86XK_AudioRaiseVolume), spawn "amixer -q set Capture,0 1000+")
    -- Ctrl+XF86AudioLowerVolume=microphone lower volume
    , ((controlMask, xF86XK_AudioLowerVolume), spawn "amixer -q set Capture,0 1000-")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm .|. shiftMask, xK_h), sendMessage $ JumpToLayout "tiled")
    , ((modm .|. shiftMask, xK_v), sendMessage $ JumpToLayout "Mirror Tall")
    , ((modm .|. shiftMask, xK_f), sendMessage $ JumpToLayout "Full")
    -- , ((modm .|. shiftMask, xK_t), sendMessage $ JumpToLayout "Tabbed Simplest")

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Shrink and expand ratio between the secondary panes, for the ResizableTall layout
    , ((modm .|. shiftMask,               xK_h), sendMessage MirrorShrink)
    , ((modm .|. shiftMask,               xK_l), sendMessage MirrorExpand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.

    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")

    , ((0, xK_Print), spawn "scrot") -- 0 means no extra modifier key needs to be pressed in this case.
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
      [((m .|. modm, key), f sc)
      | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
      -- Order screen by physical order instead of arbitrary numberings.
      , (f, m) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]

altMask :: KeyMask
altMask = mod1Mask 

baseXPConfig :: XPConfig
baseXPConfig = def
      { font                = myFont
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#ff8a18"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = baseXPKeymap
      , position            = Top
     -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Just 5      -- set to 'Just 5' for 5 rows
      }

sshXPConfig :: XPConfig
sshXPConfig = baseXPConfig
      { autoComplete        = Nothing
      }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.

baseXPKeymap :: Data.Map.Map (KeyMask,KeySym) (XP ())
baseXPKeymap = Data.Map.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh def 
        { modMask = mod4Mask
        , manageHook = manageDocks <+> myManageHook
        , layoutHook = myLayout
        , workspaces = map fst myWorkspaces        
        , startupHook = myStartupHook
        , handleEventHook = handleEventHook def <+> docksEventHook
        , keys = \c -> azertyKeys c `Data.Map.union` myKeys c
        , logHook = dynamicLogWithPP myPP {
                                          ppOutput = hPutStrLn xmproc
                                          }
                        >> historyHook
        , terminal = "urxvt"
        -- This is the color of the borders of the windows themselves.
        , normalBorderColor  = "#2f3d44"
        , focusedBorderColor = "#ff8a18"
        , borderWidth = 2
        }
        `additionalKeysP`
        [
          ("M1-<Space>", spawn "rofi -show run")
          , ("C-M1-<Space>", spawn "rofi -modi 'clipboard:greenclip print' -show clipboard")
          -- Restart xmonad. This is the same keybinding as from i3
          , ("M-S-c", spawn "xmonad --recompile; xmonad --restart")
          , ("M-S-q", kill)
          , ("M-S-<Return>", shellPrompt baseXPConfig)
          , ("M-S-a", sshPrompt sshXPConfig)
          , ("M1-<Tab>", nextMatch History (return True))
          , ("M-<Return>", spawn "urxvt")
          -- Make it really hard to mispress...
          , ("M-M1-S-e", io (exitWith ExitSuccess))
          , ("M-S-l", spawn "i3lock")
          , ("M-M1-S-s", spawn "i3lock && systemctl suspend")
          , ("M-M1-S-h", spawn "i3lock && systemctl hibernate")
        ] 
        `additionalMouseBindings`
        [ ((mod4Mask, button4), (\w -> windows W.focusUp))
        , ((mod4Mask, button5), (\w -> windows W.focusDown))
        ]
