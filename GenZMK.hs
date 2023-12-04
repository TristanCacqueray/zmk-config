-- Copyright (c) 2023 Tristan de Cacqueray
-- SPDX-License-Identifier: MIT
--
-- eval with: ghcid GenZMK.hs --test main
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

-- {-# OPTIONS_GHC -Wall #-}

module GenZMK where

import Data.Char
import Data.List (intercalate)

data ActionType = SwitchWS | Switch | Start | Open | Close | NA | Split | AUp | ALeft | ADown | ARight | Click | Mice | PgUp | PgDn
    deriving (Eq, Show)

data Dir = DUp | DLeft | DDown | DRight
    deriving (Eq)

instance Show Dir where
    show = \case
        DUp -> "up"
        DLeft -> "left"
        DDown -> "down"
        DRight -> "right"

renderIcon :: ActionType -> String
renderIcon = \case
    SwitchWS -> "💻 "
    Switch -> "⇄ "
    Start -> "🚀 "
    Open -> "📂 "
    Close -> "⌧ "
    Split -> "✂ "
    AUp -> "↑ "
    ALeft -> "← "
    ADown -> "↓ "
    ARight -> "→ "
    Click -> "🖟 "
    Mice -> "🕹 "
    PgUp -> "⇞ "
    PgDn -> "⇟ "
    NA -> ""

data Action = A
    { icon :: ActionType
    , desc :: String
    , kbd :: String
    }
    deriving (Eq, Show)

data KeyBinding = K
    { base :: Action
    , shifted :: Action
    , meta :: Action
    }

-- | Key not defined
pattern N, NN, NNN :: KeyBinding
pattern N = K X X X
pattern NN = N
pattern NNN = N

pattern X :: Action
pattern X = A NA "" ""

renderName :: Action -> String
renderName action = "em_" <> verb <> map replaceChar action.desc
  where
    replaceChar = \case
        '-' -> '_'
        other -> other
    verb = case action.icon of
        SwitchWS -> "sw_"
        Switch -> "s_"
        Start -> "start_"
        Open -> "open_"
        Close -> "close_"
        Split -> "cut_"
        AUp -> "up_"
        ALeft -> "left_"
        ADown -> "down_"
        ARight -> "right_"
        NA -> ""

pad :: Int -> String -> String
pad w s = s <> replicate (w - length s) ' '

renderKey :: String -> String
renderKey [c]
    | c >= '0' && c <= '9' = "N" <> pure c
    | c >= 'a' && c <= 'z' = pure $ toUpper c
    | otherwise = pure c
renderKey s = s

renderKeyBinding :: Action -> String
renderKeyBinding action = case action.kbd of
    "" -> "&none"
    "C-SPC" -> "&kp LC(SPACE)"
    'M' : '-' : x : [] -> "&kp LA(" <> pure x <> ")"
    '&' : rest -> '&' : rest
    other -> case keyMacro action of
        Nothing -> "&kp " <> other
        Just _ -> '&' : renderName action

keyMacro :: Action -> Maybe String
keyMacro action = case action.kbd of
    'C' : '-' : 'x' : ' ' : key -> Just $ "&kp LC(X) &kp " <> renderKey key
    'C' : '-' : 'c' : ' ' : 'p' : ' ' : key -> Just $ "&kp LC(C) &kp P &kp " <> renderKey key
    _other -> Nothing

-- ╭─────────────────────────┬─────────────────────────╮
-- │ LT5 LT4 LT3 LT2 LT1 LT0 │ RT0 RT1 RT2 RT3 RT4 RT5 │
-- │ LM5 LM4 LM3 LM2 LM1 LM0 │ RM0 RM1 RM2 RM3 RM4 RM5 │
-- │ LB5 LB4 LB3 LB2 LB1 LB0 │ RB0 RB1 RB2 RB3 RB4 RB5 │
-- ╰───────────╮ LH2 LH1 LH0 │ RH0 RH1 RH2 ╭───────────╯
--             ╰─────────────┴─────────────╯

type Layout = [[KeyBinding]]
template :: Layout
template =
    [ [N, N, N, N, N, N, N, N, N, N, N, N]
    , [N, N, N, N, N, N, N, N, N, N, N, N]
    , [N, N, N, N, N, N, N, N, N, N, N, N]
    , [N, N, N, N, N, N]
    ]

renderMods :: Layout -> String
renderMods layout = unlines $ filter (/= "") $ concat $ map renderMod (concat layout)

renderMod :: KeyBinding -> [String]
renderMod k =
    base : case (k.shifted, k.meta) of
        (X, X) -> []
        _ -> []
  where
    base = case keyMacro k.base of
        Just macro -> "ZMK_BEHAVIOR(" <> pad 25 (renderName k.base) <> ", macro, bindings = <" <> macro <> ">; wait-ms = <0>; tap-ms = <5>;)"
        Nothing -> ""

renderTemplate :: (String, Layout) -> String
renderTemplate (name, layout) = unlines [mods, mk_layout]
  where
    mods = ""
    mk_layout = unlines ["/ {", "  keymap {", "    compatible = \"zmk,keymap\";", "    layer_" <> name <> " {", "      bindings = <", bindings <> "      >;", "    };", "  };", "};"]
    bindings = unlines $ map (mappend "        ") $ map renderRow layout
    renderRow row = "  " <> intercalate " " (map renderLayoutKey row)
    renderLayoutKey k
        | k.shifted /= X = "&mod_todo"
        | otherwise = renderKeyBinding k.base

svg_ width height child = "<svg height=\"" <> show height <> "px\" width=\"" <> show width <> "px\" viewBox=\"0 0 " <> show width <> " " <> show height <> "\" xmlns=\"http://www.w3.org/2000/svg\">" <> child <> "</svg>"
rect_ width height x y = "<rect style=\"fill:none;stroke:#c3c3c3\" width=\"" <> show width <> "\" height=\"" <> show height <> "\" x=\"" <> show x <> "\" y=\"" <> show y <> "\"></rect>"
text_ sz txt x y = "<text style=\"font-size:" <> show sz <> "px;font-family:'monospace'\" x=\"" <> show x <> "\" y=\"" <> show y <> "\">" <> txt <> "</text>"

renderDoc :: [(String, Layout)] -> String
renderDoc layouts = svg_ svgWidth svgHeight $ unlines $ boards
  where
    ((svgWidth, svgHeight), boards) = foldl go ((0, -1 * bpad), []) layouts
    go ((prevWidth, prevHeight), boards) layout =
        let ((curWidth, curHeight), board) = renderBoard (prevHeight + bpad) layout
         in ((max curWidth prevWidth, curHeight + bpad + prevHeight), board : boards)
    bpad = 17

renderBoard :: Int -> (String, Layout) -> ((Int, Int), String)
renderBoard startY (name, layout) = (svgDim, unlines $ concat board)
  where
    svgDim = (svgWidth, svgHeight)
    svgWidth = length (layout !! 0) * (width + kpad) + hpad + 2 * kpad
    svgHeight = length layout * (height + kpad) + 2 * kpad
    width = 100
    height = 60
    kpad = 3
    hpad = 10
    textHeight = (height - 10) `div` 3

    layoutTitle = [text_ 24 ("Layer: " <> name) (kpad * 3) (startY + svgHeight - 30)]
    board = [renderRow 0, renderRow 1, renderRow 2, renderRow 3, layoutTitle]
    maxWidth = length $ head layout
    renderRow row = renderKey <$> zip [0 ..] rowKeys
      where
        renderKey (col, key) = rect_ width height x y <> baseKey
          where
            baseKey = text_ 12 (renderIcon key.base.icon <> key.base.desc) (kpad + x) (2 * textHeight + y)
            x = kpad + thumbsPad + halfPad + col * (width + kpad)
            y = startY + kpad + row * (height + kpad)
            halfPad = if col >= half then hpad else 0
            thumbsPad = if length rowKeys < 7 then ((maxWidth - length rowKeys) `div` 2) * (width + kpad) else 0

        render key = pad 8 key.base.desc
        half = length rowKeys `div` 2
        rowKeys = layout !! row

wmLayout :: Layout
wmLayout =
    [ [NN, N, w, e, r, t, y, u, i, o, p, N]
    , [lm, a, s, d, f, g, h, j, k, l, ψ, N]
    , [NN, z, x, c, v, b, n, m, μ, dot, λ, N]
    , [tlf, tlm, tlp, trp, trm, trf]
    ]
  where
    (w, r, t, y, a, g, n, λ, tlf, tlm, tlp, trf) =
        (N, N, N, N, N, N, N, N, N, N, N, N)
    m = K (A SwitchWS "1" "&kp LG(A)") X X
    μ = K (A SwitchWS "2" "&kp LG(S)") X X
    dot = K (A SwitchWS "3" "&kp LG(D)") X X
    j = K (A SwitchWS "code" "&kp LG(F)") X X
    k = K (A SwitchWS "mail" "&kp LG(G)") X X
    l = K (A SwitchWS " web" "&kp LG(H)") X X
    u = K (A SwitchWS "7" "&kp LG(J)") X X
    i = K (A SwitchWS "8" "&kp LG(K)") X X
    o = K (A SwitchWS "9" "&kp LG(L)") X X
    lm = K (A Switch "win" "&kp LG(TAB)") X X

    h = K (A Start "terminal" "&kp LG(RET)") X X

    ψ = K (A Open "proj-file" "C-c p f") X X
    p = K (A Open "project" "C-c p p") X X
    b = K (A Switch "buffer" "C-x b") X X
    z = K (A Close "win  " "C-x 0") X X
    x = K (A Close "other-win " "C-x 1") X X
    c = K (A Split "horiz" "C-x 2") X X
    v = K (A Split "vert" "C-x 3") X X

    e = K (A AUp "win" "&kp LS(UP)") X X
    s = K (A ALeft "win" "&kp LS(LEFT)") X X
    d = K (A ADown "win" "&kp LS(DOWN)") X X
    f = K (A ARight "win" "&kp LS(RIGHT)") X X

    trp = K (A NA "set-mark" "&kp LC(SPACE)") X X

trm = K (A NA "to-def" "&to DEFAULT") X X
ctr = K (A NA "to-ctrl" "&to CTRL") X X
tl = K (A NA "SHIFT" "&kp LSHIFT") X X

retk = K (A NA "RETURN" "&kp RET") X X

metak = K (A NA "META" "&kp LMETA") X X
ctrlk = K (A NA "CTRL" "&kp LCTRL") X X
altk = K (A NA "ALT" "&kp LALT") X X

miceLayout :: Layout
miceLayout =
    [ [trm, N, w, e, r, N, N, u, i, o, N, N]
    , [ctr, N, s, d, f, g, h, j, k, l, N, N]
    , [NNN, N, N, c, v, b, n, m, N, N, N, retk]
    , [metak, N, tl, u, trm, N]
    ]
  where
    (c, v) = (ctrlk, altk)
    lclk = "&mkp LCLK"
    u = K (A Click "clk-left" lclk) X X
    o = K (A Click "clk-right" "&mkp RCLK") X X
    m = K (A Click "clk-mid" "&mkp MCLK") X X

    mice dir = A Mice (show dir) binding
      where
        binding = "&mmv MOVE_" <> axe <> "(" <> sign <> "1250)"
        axe | dir `elem` [DUp, DDown] = "VERT" | otherwise = "HOR"
        sign | dir `elem` [DLeft, DDown] = "-" | otherwise = ""
    i = K (mice DUp) X X
    j = K (mice DLeft) X X
    k = K (mice DDown) X X
    l = K (mice DRight) X X

    scroll dir = A act dirName binding
      where
        binding = "&mwh SCROLL_VERT(" <> sign <> "10)"
        sign | dir == DDown = "-" | otherwise = ""
        dirName | dir == DDown = "scroll-dn" | otherwise = "scroll-up"
        act | dir == DDown = PgDn | otherwise = PgUp
    h = K (scroll DUp) X X
    n = K (scroll DDown) X X

    w = K (A ALeft "back-hist" "&kp LA(LEFT)") X X
    r = K (A ARight "fwd-hist" "&kp LA(RIGHT)") X X

    e = K (A AUp "" "&kp UP") X X
    s = K (A ALeft "" "&kp LEFT") X X
    d = K (A ADown "" "&kp DOWN") X X
    f = K (A ARight "" "&kp RIGHT") X X

    g = K (A PgUp "pg-up" "&kp PG_UP") X X
    b = K (A PgDn "pg-dn" "&kp PG_DN") X X

{-
_emacs :: Layout
_emacs = []
  where
    _i = K (A "↑line" "UP") X X
    _j = K (A "←char" "LEFT") X X
    _k = K (A "↓line" "DOWN") X X
    _l = K (A "→char" "RIGHT") X X
    _u = K (A "←word" "M-B") (A "←¶" "M-P") X
    _o = K (A "→word" "M-F") (A "→¶" "M-N") X

    _ll = K (A "meta" "LMETA") X X
    _lm = K (A "shift" "LSHIFT") X X
    _rm = K (A "set-mark" "C-SPC") X X
    _rl = K (A "mo-fn" "&mo FUN") X X
    _rf = K (A "delete-window" "C-x 0") X X
-}

main :: IO ()
main = do
    putStrLn doc
    writeFile "layers.svg" doc
    putStrLn gen
    writeFile "./config/codegen.dtsi" gen
  where
    layouts = [("wm", wmLayout), ("mice", miceLayout)]
    doc = renderDoc layouts
    gen =
        unlines $
            ["// Generated config with GenZMK", "// SPDX-License-Identifier: MIT"]
                <> (renderMods . snd <$> layouts)
                <> (renderTemplate <$> layouts)
