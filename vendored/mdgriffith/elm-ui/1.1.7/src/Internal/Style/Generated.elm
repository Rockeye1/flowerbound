module Internal.Style.Generated exposing (Var(..), classes, vars, stylesheet, lineHeightAdjustment)

{-| This file is generated via 'npm run stylesheet' in the elm-ui repository -}

{- BEGIN COPY -}


classes =
    { root = "elm-ui-root"
    , any = "s"
    , el = "e"
    , row = "r"
    , column = "c"
    , page = "pg"
    , paragraph = "p"
    , text = "t"
    , grid = "g"
    , imageContainer = "ic"
    , nowrap = "nowrp"
    , transform = "move"
    , ellipses = "ellip"

    -- widhts/heights
    , widthFill = "wf"
    , widthContent = "wc"
    , widthExact = "we"
    , widthBounded = "wb"
    , heightFill = "hf"
    , heightContent = "hc"
    , heightExact = "he"
    , heightBounded = "hb"

    -- nearby elements
    , hasNearby = "hnb"
    , nearby = "nb"
    , above = "a"
    , below = "b"
    , onRight = "or"
    , onLeft = "ol"
    , inFront = "fr"
    , behind = "bh"
    , hasBehind = "hbh"

    -- alignments
    , alignTop = "at"
    , alignBottom = "ab"
    , alignRight = "ar"
    , alignLeft = "al"
    , alignCenterX = "cx"
    , alignCenterY = "cy"
    , alignedHorizontally = "ah"

    -- space evenly
    , spacing = "spc"
    , spaceEvenly = "sev"
    , padding = "pad"

    -- content alignments
    , contentTop = "ct"
    , contentBottom = "cb"
    , contentRight = "cr"
    , contentLeft = "cl"
    , contentCenterX = "ccx"
    , contentCenterY = "ccy"

    -- selection
    , noTextSelection = "notxt"
    , cursorPointer = "cptr"
    , cursorGrab = "grab"
    , cursorGrabbing = "grabbing"
    , cursorText = "ctxt"

    -- pointer events
    , passPointerEvents = "ppe"
    , capturePointerEvents = "cpe"
    , transparent = "clr"
    , opaque = "oq"
    , overflowHidden = "oh"

    -- special state classes
    , hover = "hv"
    , focus = "fcs"
    , focusedWithin = "focus-within"
    , active = "atv"

    --scrollbars
    , scrollbars = "sb"
    , scrollbarsX = "sbx"
    , scrollbarsY = "sby"
    , clip = "cp"
    , clipX = "cpx"
    , clipY = "cpy"

    -- text weight
    , sizeByCapital = "cap"
    , fullSize = "fs"
    , italic = "i"
    , strike = "sk"
    , underline = "u"
    , textJustify = "tj"
    , textJustifyAll = "tja"
    , textCenter = "tc"
    , textRight = "tr"
    , textLeft = "tl"

    -- line height
    , lineHeightPrefix = "lh"

    -- text alignment
    , transition = "ts"

    -- inputText
    , inputReset = "irs"
    , inputText = "it"
    , inputTextInputWrapper = "itw"
    , inputTextParent = "itp"
    , inputMultiline = "iml"
    , inputMultilineParent = "imlp"
    , inputMultilineFiller = "imlf"
    , inputMultilineWrapper = "implw"
    , inputLabel = "lbl"
    , slider = "sldr"

    -- link
    , link = "lnk"
    , fontAdjusted = "f-adj"
    , textGradient = "tgrd"
    , stickyTop = "stick-top"
    , stickyLeft = "stick-left"
    , stickyBottom = "stick-bottom"

    -- animation triggers
    , onHovered = "on-hovered"
    , onFocused = "on-focused"
    , onFocusedWithin = "on-focused-within"
    , onPressed = "on-activated"
    , onRendered = "on-rendered"
    , onDismout = "on-dismount"

    --
    , trigger = "ui-trigger"
    }


type Var
    = Var String


vars =
    { spaceX = Var "space-x"
    , spaceY = Var "space-y"
    , scale = Var "scale"
    , moveX = Var "move-x"
    , moveY = Var "move-y"
    , rotate = Var "rotate"
    , heightFill = Var "height-fill"
    , widthFill = Var "width-fill"
    , padLeft = Var "pad-left"
    , padRight = Var "pad-right"
    , padTop = Var "pad-top"
    , padBottom = Var "pad-bottom"
    , borderLeft = Var "border-left"
    , borderRight = Var "border-right"
    , borderTop = Var "border-top"
    , borderBottom = Var "border-bottom"
    , sliderWidth = Var "slider-width"
    , sliderHeight = Var "slider-height"

    --
    , fontSizeFactor = Var "font-size-factor"
    , vacuumTop = Var "vacuum-top"
    , vacuumBottom = Var "vacuum-bottom"
    , visibleTop = Var "visible-top"
    , visibleBottom = Var "visible-bottom"
    }


lineHeightAdjustment : Int -> String
lineHeightAdjustment i =
    let
        -- offset would be 5 if the line-height is 1.05
        offsetInt =
            i * 5

        lineHeightOffset =
            toFloat offsetInt / 100

        offset =
            -- 0.05 line height
            -- But we need to express it as a percentage of the *existing* lineheight.
            lineHeightOffset
                / (1 + lineHeightOffset)

        offsetString =
            String.fromFloat (offset / 2)
    in
    "-" ++ offsetString ++ "lh"



stylesheet : String
stylesheet = """input[type="search"],
input[type="search"]::-webkit-search-decoration,
input[type="search"]::-webkit-search-cancel-button,
input[type="search"]::-webkit-search-results-button,
input[type="search"]::-webkit-search-results-decoration {
  -webkit-appearance:none;
}
input[type=range].sldr {
    -webkit-appearance: none;
    background: transparent;
    opacity: 0;
}
            
input[type=range].sldr::-moz-range-track {
    background: transparent;
}
input[type=range].sldr::-ms-track {
    background: transparent;
}
input[type=range].sldr::-webkit-slider-runnable-track {
    background: transparent;
}
            @keyframes show-redraw { 0% { background-color:red; }}@keyframes on-hovered { from {} to {} }
@keyframes on-focused { from {} to {} }
@keyframes on-activated { from {} to {} }
@keyframes on-rendered { from {} to {} }
@keyframes on-dismount { from {} to {} }html,body{height:100%;padding:0;margin:0;}.irs{appearance:none;-webkit-appearance:none;-moz-appearance:none;}.s.e.ic{display:block;object-fit:cover;width:max-content;}.s:focus{outline:none;}.explain{outline:6px solid rgb(174, 121, 15) !important;}.explain > .s{outline:4px dashed rgb(0, 151, 167) !important;}.explain *{animation:show-redraw 0.4s ease;}.elm-ui-root{width:100%;height:auto;min-height:100%;font-size:16px;font-family:"Open Sans", sans-serif;line-height:1.4;}.elm-ui-root{position:relative;z-index:0;}.elm-ui-root.s.hf{height:100%;}.elm-ui-root.s.hf > .hf{height:100%;}.elm-ui-root > .fr.nb{position:fixed;z-index:20;}.hnb{position:relative;}.hnb > .s{position:relative;}li.s.e{display:list-item;}.ui-trigger{position:absolute;}.nb{position:relative;border:none;display:flex;flex-direction:row;flex-basis:auto;border-radius:inherit;}.nb.e{display:flex;flex-direction:column;}.nb.e.hbh{z-index:0;}.nb.e.hbh > .bh{z-index:-1;}.nb.e > .hc{height:auto;}.nb.e > .hf{flex-grow:1;max-height:100%;}.nb.e > .he{flex-shrink:0;}.nb.e > .wf:not(.ar, .al, .cx){width:100%;}.nb.e > .wf.wb{width:100%;}.nb.e > .wc{align-self:flex-start;}.nb.e.ccx{align-items:center;}.nb.e > .s.cx{align-self:center;}.nb.e.ccy{justify-content:center;}.nb.e > .s.cy{margin-top:auto !important;margin-bottom:auto !important;}.nb.e.ct{justify-content:flex-start;}.nb.e > .s.at{margin-bottom:auto !important;margin-top:0 !important;}.nb.e.cb{justify-content:flex-end;}.nb.e > .s.ab{margin-top:auto !important;margin-bottom:0 !important;}.nb.e.cr{align-items:flex-end;}.nb.e > .s.ar{align-self:flex-end;}.nb.e.cl{align-items:flex-start;}.nb.e > .s.al{align-self:flex-start;}.nb.a{position:absolute;bottom:100%;left:0;width:100%;z-index:20;margin:0 !important;pointer-events:none;}.nb.a > .hf{height:auto;}.nb.a > .wf:not(.ar, .al, .cx){width:100%;}.nb.a > *{pointer-events:auto;}.nb.b{position:absolute;bottom:0;left:0;height:0;width:100%;z-index:20;margin:0 !important;pointer-events:none;}.nb.b > *{pointer-events:auto;}.nb.b > .hf{height:auto;}.nb.or{position:absolute;left:100%;top:0;height:100%;margin:0 !important;z-index:20;pointer-events:none;}.nb.or > *{pointer-events:auto;}.nb.ol{position:absolute;right:100%;top:0;height:100%;margin:0 !important;z-index:20;pointer-events:none;}.nb.ol > *{pointer-events:auto;}.nb.fr{position:absolute;width:100%;height:100%;left:0;top:0;margin:0 !important;pointer-events:none;}.nb.fr > *{pointer-events:auto;}.nb.bh{position:absolute;width:100%;height:100%;left:0;top:0;margin:0 !important;z-index:0;pointer-events:none;}.nb.bh > *{pointer-events:auto;}.e{flex:0 0 0px;align-items:flex-start;min-height:min-content;display:flex;flex-direction:column;}.e.we{flex-shrink:0;}button.s{background-color:transparent;text-align:start;}.s{border:none;flex-shrink:1;flex-basis:auto;display:flex;flex-direction:row;resize:none;box-sizing:border-box;margin:0;padding:0;border-width:0;border-style:solid;min-width:0;font-size:inherit;color:inherit;font-family:inherit;font-weight:inherit;font-feature-settings:inherit;text-decoration:none;font-style:inherit;}.s.on-hovered:hover > .ui-trigger{animation:on-hovered 1ms;}.s.on-focused:focus > .ui-trigger{animation:on-focused 1ms;}.s.on-focused-within:focus-within > .ui-trigger{animation:on-focused 1ms;}.s.on-activated:active > .ui-trigger{animation:on-activated 1ms;}.s.on-rendered > .ui-trigger{animation:on-rendered 1ms;}.s.on-dismount > .ui-trigger{animation:on-dismount 31449600s;}.s.stick-top{position:sticky;top:0;}.s.stick-bottom{position:sticky;bottom:-1px;}.s.stick-left{position:sticky;left:0;}.s.tgrd{-webkit-background-clip:text;-webkit-text-fill-color:transparent;}.s.f-adj{font-size:calc(1em * var(--font-size-factor));}.s.ellip .t{text-overflow:ellipsis;white-space:nowrap;overflow:hidden;}.s.notxt{user-select:none;}.s.cptr{cursor:pointer;}.s.grab{cursor:grab;}.s.grabbing{cursor:grabbing;}.s.ctxt{cursor:text;}.s.ppe{pointer-events:none !important;}.s.cpe{pointer-events:auto !important;}.s.clr{opacity:0;}.s.oq{opacity:1;}.s.hvclr:hover{opacity:0;}.s.hvoq:hover{opacity:1;}.s.fcsclr:focus{opacity:0;}.s.fcsoq:focus{opacity:1;}.s.atvclr:active{opacity:0;}.s.atvoq:active{opacity:1;}.s.ts{transition:transform 160ms, opacity 160ms, filter 160ms, background-color 160ms, color 160ms, font-size 160ms;}.s.sb{overflow:auto;flex-shrink:1;}.s.sb.c{flex-shrink:1;flex-basis:auto;}.s.sb.e{flex-shrink:1;flex-basis:auto;}.s.sbx{overflow-x:auto;}.s.sbx.r{flex-shrink:1;}.s.sby{overflow-y:auto;}.s.sby.c{flex-shrink:1;flex-basis:auto;}.s.sby.e{flex-shrink:1;flex-basis:auto;}.s.cp{overflow:hidden;}.s.cpx{overflow-x:hidden;}.s.cpy{overflow-y:hidden;}.s.wc{width:auto;}.s.t{display:inline-block;max-width:100%;}.s.e{display:flex;flex-direction:column;}.s.e.hbh{z-index:0;}.s.e.hbh > .bh{z-index:-1;}.s.e > .hc{height:auto;}.s.e > .hf{flex-grow:1;max-height:100%;}.s.e > .he{flex-shrink:0;}.s.e > .wf:not(.ar, .al, .cx){width:100%;}.s.e > .wf.wb{width:100%;}.s.e > .wc{align-self:flex-start;}.s.e.ccx{align-items:center;}.s.e > .s.cx{align-self:center;}.s.e.ccy{justify-content:center;}.s.e > .s.cy{margin-top:auto !important;margin-bottom:auto !important;}.s.e.ct{justify-content:flex-start;}.s.e > .s.at{margin-bottom:auto !important;margin-top:0 !important;}.s.e.cb{justify-content:flex-end;}.s.e > .s.ab{margin-top:auto !important;margin-bottom:0 !important;}.s.e.cr{align-items:flex-end;}.s.e > .s.ar{align-self:flex-end;}.s.e.cl{align-items:flex-start;}.s.e > .s.al{align-self:flex-start;}.s.r{display:flex;flex-direction:row;align-items:center;}.s.r.wf:not(.ar, .al, .cx) > .wf:not(.ar, .al, .cx){flex-basis:0%;}.s.r > .s{flex-basis:auto;flex-shrink:1;}.s.r > .s.wf:not(.ar, .al, .cx){flex-grow:1;}.s.r > .s.wf:has(> :where(.we, .wb)){min-width:min-content;}.s.r > .s.cp.wf:not(.ar, .al, .cx){min-width:auto;}.s.r > .s.cpx.wf:not(.ar, .al, .cx){min-width:auto;}.s.r > .s.we{flex-shrink:0;}.s.r > .hf:not(.at, .ab, .cy, .hb){align-self:stretch;}.s.r > .hf.hb{height:max(2000px, 100vh);}.s.r.ccx{justify-content:center;}.s.r > .s.cx{margin:0 auto;}.s.r.ccy{align-items:center;}.s.r > .s.cy{margin:auto 0;}.s.r.ct{align-items:flex-start;}.s.r > .s.at{margin-bottom:auto;}.s.r.cb{align-items:flex-end;}.s.r > .s.ab{margin-top:auto;}.s.r.cr{justify-content:flex-end;}.s.r > .s.ar{margin-left:auto;}.s.r.cl{justify-content:flex-start;}.s.r > .s.al{margin-right:auto;}.s.r.sev{justify-content:space-between;}.s.r.lbl{align-items:baseline;}.s.c{display:flex;flex-direction:column;align-content:flex-start;}.s.c.we{flex-shrink:0;}.s.c > .s{min-height:min-content;}.s.c > .s.he{flex-basis:auto;flex-shrink:0;}.s.c > .s.cp{flex-basis:auto;}.s.c > .s.sb{flex-basis:auto;}.s.c > .s.wb.wf{width:100%;}.s.c > .hf{flex-grow:1;max-height:100%;}.s.c > .wf:not(.ar, .al, .cx){width:100%;}.s.c > .wc{align-self:flex-start;}.s.c.ccx{align-items:center;}.s.c > .s.cx{align-self:center;}.s.c.ccy{justify-content:center;}.s.c > .s.cy{margin:auto 0;}.s.c.ct{justify-content:flex-start;}.s.c > .s.at{margin-bottom:auto;}.s.c.cb{justify-content:flex-end;}.s.c > .s.ab{margin-top:auto;}.s.c.cr{align-items:flex-end;}.s.c > .s.ar{align-self:flex-end;}.s.c.cl{align-items:flex-start;}.s.c > .s.al{align-self:flex-start;}.s.c.sev{justify-content:space-between;}.s.g{display:-ms-grid;display:grid;}.s.g > .gp > .s{width:100%;}.s.g > .s.cx{align-items:center;}.s.g > .s.cy{justify-content:center;}.s.g > .s.at{justify-content:flex-start;}.s.g > .s.ab{justify-content:flex-end;}.s.g > .s.ar{align-items:flex-end;}.s.g > .s.al{align-items:flex-start;}.s.pg{display:block;}.s.pg > .s:first-child{margin:0 !important;}.s.pg > .s.al:first-child + .s{margin:0 !important;}.s.pg > .s.ar:first-child + .s{margin:0 !important;}.s.pg > .s.ar{float:right;}.s.pg > .s.ar::after{content:"";display:table;clear:both;}.s.pg > .s.al{float:left;}.s.pg > .s.al::after{content:"";display:table;clear:both;}.s.iml{white-space:pre-wrap !important;height:100%;width:100%;background-color:transparent !important;border-color:transparent !important;}.s.implw.e{flex-basis:auto;}.s.imlp{white-space:pre-wrap !important;cursor:text;}.s.imlp > .imlf{white-space:pre-wrap !important;color:transparent;}.s.p{display:block;overflow-wrap:break-word;}.s.p .t{display:inline;}.s.p .t::after{content:none;}.s.p .t::before{content:none;}.s.p::after{content:" ";margin-top:calc((1lh - 1cap) / -2);display:table;}.s.p::before{content:" ";margin-bottom:calc((1lh - 1cap) / -2);display:table;}.s.p.hbh{z-index:0;}.s.p.hbh > .bh{z-index:-1;}.s.p .t{display:inline;}.s.p > .p{display:inline;}.s.p > .p::after{content:none;}.s.p > .p::before{content:none;}.s.p .e{display:inline;}.s.p .e.we{display:inline-block;}.s.p .e.nb{display:flex;}.s.p .e > .t{display:inline;}.s.p > .r{display:inline;}.s.p > .c{display:inline-flex;}.s.p > .g{display:inline-grid;}.s.p > .s.ar{float:right;}.s.p > .s.al{float:left;}.s.hidden{display:none;}.s.tj{text-align:justify;}.s.tja{text-align:justify-all;}.s.tc{text-align:center;align-items:center;}.s.tr{text-align:end;align-items:flex-end;}.s.tl{text-align:start;}.s.i{font-style:italic;}.s.sk{text-decoration:line-through;}.s.u{text-decoration:underline;text-decoration-skip-ink:auto;text-decoration-skip:ink;}.s.u.sk{text-decoration:line-through underline;text-decoration-skip-ink:auto;text-decoration-skip:ink;}.itp{padding:0 !important;border-width:0 !important;transform:none;}.it{background-color:rgba(255,255,255,0);}.sldr{flex:none;flex-direction:unset;padding:0;margin:0;}input[type="range"].sldr::-moz-range-thumb{width:16px;height:16px;}input[type="range"].sldr::-webkit-slider-thumb{width:16px;height:16px;}input[type="range"].sldr::-ms-thumb{width:16px;height:16px;}"""
