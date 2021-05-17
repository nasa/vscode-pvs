// this file replicates symbols.json form the vscode-pvs root folder
export interface Symbol {
    description: string,
    prefix: string,
    scope: string,
    body: string[]
};
export const symbols: Symbol[] = [
    {
        "description": "forall",
        "prefix": "\\forall ∀",
        "scope": "pvs",
        "body": [ "∀" ]
    }, {
        "description": "exists",
        "prefix": "\\exists ∃",
        "scope": "pvs",
        "body": [ "∃" ]
    }, {
        "description": "implies",
        "prefix": "\\implies ⇒",
        "scope": "pvs",
        "body": [ "⇒" ]
    }, {
        "description": "if and only if",
        "prefix": "\\iff ⇔",
        "scope": "pvs",
        "body": [ "⇔" ]
    }, {
        "description": "if and only if",
        "prefix": "\\Leftrightarrow ⇔",
        "scope": "pvs",
        "body": [ "⇔" ]
    }, {
        "description": "long left right double arrow",
        "prefix": "\\Longleftrightarrow ⟺",
        "scope": "pvs",
        "body": [ "⟺" ]
    }, {
        "description": "ring",
        "prefix": "\\o ∘",
        "scope": "pvs",
        "body": [ "∘" ]
    }, {
        "description": "ring",
        "prefix": "\\ring ∘",
        "scope": "pvs",
        "body": [ "∘" ]
    }, {
        "description": "ring",
        "prefix": "\\circ ∘",
        "scope": "pvs",
        "body": [ "∘" ]
    }, {
        "description": "ring plus",
        "prefix": "\\oplus ⊕",
        "scope": "pvs",
        "body": [ "⊕" ]
    }, {
        "description": "ring plus",
        "prefix": "\\ringplus ⊕",
        "scope": "pvs",
        "body": [ "⊕" ]
    }, {
        "description": "circle plus",
        "prefix": "\\circleplus ⊕",
        "scope": "pvs",
        "body": [ "⊕" ]
    }, {
        "description": "ring minus",
        "prefix": "\\ominus ⊖",
        "scope": "pvs",
        "body": [ "⊖" ]
    }, {
        "description": "ring minus",
        "prefix": "\\ringminus ⊖",
        "scope": "pvs",
        "body": [ "⊖" ]
    }, {
        "description": "circle minus",
        "prefix": "\\circleminus ⊖",
        "scope": "pvs",
        "body": [ "⊖" ]
    }, {
        "description": "circle minus",
        "prefix": "\\circleddash ⊖",
        "scope": "pvs",
        "body": [ "⊖" ]
    }, {
        "description": "ring times",
        "prefix": "\\otimes ⊗",
        "scope": "pvs",
        "body": [ "⊗" ]
    }, {
        "description": "ring times",
        "prefix": "\\ringtimes ⊗",
        "scope": "pvs",
        "body": [ "⊗" ]
    }, {
        "description": "circle times",
        "prefix": "\\circletimes ⊗",
        "scope": "pvs",
        "body": [ "⊗" ]
    }, {
        "description": "ring dot",
        "prefix": "\\odot ⊙",
        "scope": "pvs",
        "body": [ "⊙" ]
    }, {
        "description": "ring dot",
        "prefix": "\\ringdot ⊙",
        "scope": "pvs",
        "body": [ "⊙" ]
    }, {
        "description": "circle dot",
        "prefix": "\\circledot ⊙",
        "scope": "pvs",
        "body": [ "⊙" ]
    }, {
        "description": "ring star",
        "prefix": "\\ostar ⊛",
        "scope": "pvs",
        "body": [ "⊛" ]
    }, {
        "description": "ring star",
        "prefix": "\\ringstar ⊛",
        "scope": "pvs",
        "body": [ "⊛" ]
    }, {
        "description": "circle star",
        "prefix": "\\circlestar ⊛",
        "scope": "pvs",
        "body": [ "⊛" ]
    }, {
        "description": "circle star",
        "prefix": "\\circledast ⊛",
        "scope": "pvs",
        "body": [ "⊛" ]
    }, {
        "description": "circled circle",
        "prefix": "\\circledcirc ⦾",
        "scope": "pvs",
        "body": [ "⦾" ]
    }, {
        "description": "empty set",
        "prefix": "\\oslash ⊘",
        "scope": "pvs",
        "body": [ "⊘" ]
    }, {
        "description": "empty set",
        "prefix": "\\ringslash ⊘",
        "scope": "pvs",
        "body": [ "⊘" ]
    }, {
        "description": "circle set",
        "prefix": "\\circlelash ⊘",
        "scope": "pvs",
        "body": [ "⊘" ]
    }, {
        "description": "empty set",
        "prefix": "\\emptyset ⊘",
        "scope": "pvs",
        "body": [ "⊘" ]
    }, {
        "description": "big circle plus",
        "prefix": "\\bigcircleplus ⨁",
        "scope": "pvs",
        "body": [ "⨁" ]
    }, {
        "description": "big circle times",
        "prefix": "\\bigcircletimes ⨂",
        "scope": "pvs",
        "body": [ "⨂" ]
    }, {
        "description": "big circle dot",
        "prefix": "\\bigcircledot ⨀",
        "scope": "pvs",
        "body": [ "⨀" ]
    }, {
        "description": "turnstile",
        "prefix": "\\vdash ⊢",
        "scope": "pvs",
        "body": [ "⊢" ]
    }, {
        "description": "turnstile",
        "prefix": "\\turnstile ⊢",
        "scope": "pvs",
        "body": [ "⊢" ]
    }, {
        "description": "not turnstile",
        "prefix": "\\nvdash ⊬",
        "scope": "pvs",
        "body": [ "⊬" ]
    }, {
        "description": "double turnstile",
        "prefix": "\\vDash ⊨",
        "scope": "pvs",
        "body": [ "⊨" ]
    }, {
        "description": "double turnstile",
        "prefix": "\\doubleturnstile ⊨",
        "scope": "pvs",
        "body": [ "⊨" ]
    }, {
        "description": "double turnstile",
        "prefix": "\\models ⊨",
        "scope": "pvs",
        "body": [ "⊨" ]
    }, {
        "description": "not true",
        "prefix": "\\nvDash ⊭",
        "scope": "pvs",
        "body": [ "⊭" ]
    }, {
        "description": "not equal",
        "prefix": "\\neq ≠",
        "scope": "pvs",
        "body": [ "≠" ]
    }, {
        "description": "plus minus",
        "prefix": "\\plusminus ±",
        "scope": "pvs",
        "body": [ "±" ]
    },  {
        "description": "plus minus",
        "prefix": "\\pm ±",
        "scope": "pvs",
        "body": [ "±" ]
    }, {
        "description": "minus plus",
        "prefix": "\\minusplus ∓",
        "scope": "pvs",
        "body": [ "∓" ]
    }, {
        "description": "minus plus",
        "prefix": "\\mp ∓",
        "scope": "pvs",
        "body": [ "∓" ]
    }, {
        "description": "dot plus",
        "prefix": "\\dotplus ∔",
        "scope": "pvs",
        "body": [ "∔" ]
    }, {
        "description": "times",
        "prefix": "\\times ×",
        "scope": "pvs",
        "body": [ "×" ]
    }, {
        "description": "divide",
        "prefix": "\\div ÷",
        "scope": "pvs",
        "body": [ "÷" ]
    }, {
        "description": "divideontimes",
        "prefix": "\\divideontimes ⋇",
        "scope": "pvs",
        "body": [ "⋇" ]
    }, {
        "description": "box plus",
        "prefix": "\\boxplus ⊞",
        "scope": "pvs",
        "body": [ "⊞" ]
    }, {
        "description": "box minus",
        "prefix": "\\boxminus ⊟",
        "scope": "pvs",
        "body": [ "⊟" ]
    }, {
        "description": "box times",
        "prefix": "\\boxtimes ⊠",
        "scope": "pvs",
        "body": [ "⊠" ]
    }, {
        "description": "box dot",
        "prefix": "\\boxdot ⊡",
        "scope": "pvs",
        "body": [ "⊡" ]
    }, {
        "description": "not similar to",
        "prefix": "\\nsim ≁",
        "scope": "pvs",
        "body": [ "≁" ]
    }, {
        "description": "not similar to",
        "prefix": "\\tildeslash ≁",
        "scope": "pvs",
        "body": [ "≁" ]
    }, {
        "description": "similar to",
        "prefix": "\\sim ∼",
        "scope": "pvs",
        "body": [ "∼" ]
    }, {
        "description": "similar to",
        "prefix": "\\tilde ∼",
        "scope": "pvs",
        "body": [ "∼" ]
    }, {
        "description": "backsim",
        "prefix": "\\backsim ∽",
        "scope": "pvs",
        "body": [ "∽" ]
    }, {
        "description": "backsimeq",
        "prefix": "\\backsimeq ⋍",
        "scope": "pvs",
        "body": [ "⋍" ]
    }, {
        "description": "approximately equal to",
        "prefix": "\\simeq ≃",
        "scope": "pvs",
        "body": [ "≃" ]
    }, {
        "description": "approximately equal to",
        "prefix": "\\approxeq ≊",
        "scope": "pvs",
        "body": [ "≊" ]
    }, {
        "description": "not congruent to",
        "prefix": "\\ncong ≇",
        "scope": "pvs",
        "body": [ "≇" ]
    }, {
        "description": "congruent to",
        "prefix": "\\cong ≅",
        "scope": "pvs",
        "body": [ "≅" ]
    }, {
        "description": "equivalent to",
        "prefix": "\\equiv ≡",
        "scope": "pvs",
        "body": [ "≡" ]
    }, {
        "description": "approximately equal to",
        "prefix": "\\approx ≈",
        "scope": "pvs",
        "body": [ "≈" ]
    }, {
        "description": "fallingdotseq",
        "prefix": "\\fallingdotseq ≒",
        "scope": "pvs",
        "body": [ "≒" ]
    }, {
        "description": "risingdotseq",
        "prefix": "\\risingdotseq ≓",
        "scope": "pvs",
        "body": [ "≓" ]
    }, {
        "description": "not approximately equal to",
        "prefix": "\\sapprox ≉",
        "scope": "pvs",
        "body": [ "≉" ]
    }, {
        "description": "proportional to",
        "prefix": "\\propto ∝",
        "scope": "pvs",
        "body": [ "∝" ]
    }, {
        "description": "proportional to",
        "prefix": "\\varpropto ∝",
        "scope": "pvs",
        "body": [ "∝" ]
    }, {
        "description": "therefore",
        "prefix": "\\therefore ∴",
        "scope": "pvs",
        "body": [ "∴" ]
    }, {
        "description": "because",
        "prefix": "\\because ∵",
        "scope": "pvs",
        "body": [ "∵" ]
    }, {
        "description": "logical negation",
        "prefix": "\\neg ¬",
        "scope": "pvs",
        "body": [ "¬" ]
    }, {
        "description": "logical negation",
        "prefix": "\\lnot ¬",
        "scope": "pvs",
        "body": [ "¬" ]
    }, {
        "description": "square root",
        "prefix": "\\sqrt √",
        "scope": "pvs",
        "body": [ "√" ]
    }, {
        "description": "square root",
        "prefix": "\\surd √",
        "scope": "pvs",
        "body": [ "√" ]
    }, {
        "description": "asymptotically equivalent to",
        "prefix": "\\asymp ≍",
        "scope": "pvs",
        "body": [ "≍" ]
    }, {
        "description": "geometrically equivalent to",
        "prefix": "\\Bumpeq ≎",
        "scope": "pvs",
        "body": [ "≎" ]
    }, {
        "description": "Cap",
        "prefix": "\\Cap ⋒",
        "scope": "pvs",
        "body": [ "⋒" ]
    }, {
        "description": "Cup",
        "prefix": "\\Cup ⋓",
        "scope": "pvs",
        "body": [ "⋓" ]
    }, {
        "description": "difference between",
        "prefix": "\\bumpeq ≏",
        "scope": "pvs",
        "body": [ "≏" ]
    }, {
        "description": "dot equal",
        "prefix": "\\doteq ≐",
        "scope": "pvs",
        "body": [ "≐" ]
    }, {
        "description": "dot equal dot",
        "prefix": "\\doteqdot ≑",
        "scope": "pvs",
        "body": [ "≑" ]
    }, {
        "description": "o equal",
        "prefix": "\\ringeq ≗",
        "scope": "pvs",
        "body": [ "≗" ]
    }, {
        "description": "o equal",
        "prefix": "\\circeq ≗",
        "scope": "pvs",
        "body": [ "≗" ]
    }, {
        "description": "o equal",
        "prefix": "\\oeq ≗",
        "scope": "pvs",
        "body": [ "≗" ]
    }, {
        "description": "defines",
        "prefix": "\\defs ≙",
        "scope": "pvs",
        "body": [ "≙" ]
    }, {
        "description": "defines",
        "prefix": "\\triangleq ≙",
        "scope": "pvs",
        "body": [ "≙" ]
    }, {
        "description": "eqcirc",
        "prefix": "\\eqcirc ≖",
        "scope": "pvs",
        "body": [ "≖" ]
    }, {
        "description": "bowtie",
        "prefix": "\\bowtie ⋈",
        "scope": "pvs",
        "body": [ "⋈" ]
    }, {
        "description": "bowtie",
        "prefix": "\\Join ⋈",
        "scope": "pvs",
        "body": [ "⋈" ]
    }, {
        "description": "less than or equal to",
        "prefix": "\\leq ≤",
        "scope": "pvs",
        "body": [ "≤" ]
    }, {
        "description": "greater than or equal to",
        "prefix": "\\geq ≥",
        "scope": "pvs",
        "body": [ "≥" ]
    }, {
        "description": "less than over equal to",
        "prefix": "\\leqq ≦",
        "scope": "pvs",
        "body": [ "≦" ]
    }, {
        "description": "greater than over equal to",
        "prefix": "\\geqq ≧",
        "scope": "pvs",
        "body": [ "≧" ]
    }, {
        "description": "less than over not equal to",
        "prefix": "\\lneqq ≨",
        "scope": "pvs",
        "body": [ "≨" ]
    }, {
        "description": "greater than over not equal to",
        "prefix": "\\gneqq ≩",
        "scope": "pvs",
        "body": [ "≩" ]
    }, {
        "description": "much less than",
        "prefix": "\\ll ≪",
        "scope": "pvs",
        "body": [ "≪" ]
    }, {
        "description": "much much less than",
        "prefix": "\\lll ⋘",
        "scope": "pvs",
        "body": [ "⋘" ]
    }, {
        "description": "much greater than",
        "prefix": "\\gg ≫",
        "scope": "pvs",
        "body": [ "≫" ]
    }, {
        "description": "much much greater than",
        "prefix": "\\ggg ⋙",
        "scope": "pvs",
        "body": [ "⋙" ]
    }, {
        "description": "not less than",
        "prefix": "\\nless ≮",
        "scope": "pvs",
        "body": [ "≮" ]
    }, {
        "description": "not greater than",
        "prefix": "\\ngtr ≯",
        "scope": "pvs",
        "body": [ "≯" ]
    }, {
        "description": "not less than or equal to",
        "prefix": "\\nleq ≰",
        "scope": "pvs",
        "body": [ "≰" ]
    }, {
        "description": "not greater than or equal to",
        "prefix": "\\ngeq ≱",
        "scope": "pvs",
        "body": [ "≱" ]
    }, {
        "description": "preceeds",
        "prefix": "\\prec ≺",
        "scope": "pvs",
        "body": [ "≺" ]
    }, {
        "description": "succeeds",
        "prefix": "\\succ ≻",
        "scope": "pvs",
        "body": [ "≻" ]
    }, {
        "description": "lessdot",
        "prefix": "\\lessdot ⋖",
        "scope": "pvs",
        "body": [ "⋖" ]
    }, {
        "description": "gtrdot",
        "prefix": "\\gtrdot ⋗",
        "scope": "pvs",
        "body": [ "⋗" ]
    }, {
        "description": "lesssim",
        "prefix": "\\lesssim ≲",
        "scope": "pvs",
        "body": [ "≲" ]
    }, {
        "description": "gtrsim",
        "prefix": "\\gtrsim ≳",
        "scope": "pvs",
        "body": [ "≳" ]
    }, {
        "description": "triangle right",
        "prefix": "\\triangleright ▷",
        "scope": "pvs",
        "body": [ "▷" ]
    }, {
        "description": "triangle right",
        "prefix": "\\rhd ▷",
        "scope": "pvs",
        "body": [ "▷" ]
    }, {
        "description": "not triangle right",
        "prefix": "\\ntriangleright ⋫",
        "scope": "pvs",
        "body": [ "⋫" ]
    }, {
        "description": "triangle right equal",
        "prefix": "\\trianglerighteq ⊵",
        "scope": "pvs",
        "body": [ "⊵" ]
    }, {
        "description": "not triangle right equal",
        "prefix": "\\ntrianglerighteq ⋭",
        "scope": "pvs",
        "body": [ "⋭" ]
    }, {
        "description": "triangle left",
        "prefix": "\\triangleleft ◁",
        "scope": "pvs",
        "body": [ "◁" ]
    }, {
        "description": "triangle left",
        "prefix": "\\lhd ◁",
        "scope": "pvs",
        "body": [ "◁" ]
    }, {
        "description": "triangle left equal",
        "prefix": "\\trianglelefteq ⊴",
        "scope": "pvs",
        "body": [ "⊴" ]
    }, {
        "description": "not triangle left equal",
        "prefix": "\\ntrianglelefteq ⋬",
        "scope": "pvs",
        "body": [ "⋬" ]
    }, {
        "description": "not triangle left",
        "prefix": "\\ntriangleleft ⋪",
        "scope": "pvs",
        "body": [ "⋪" ]
    }, {
        "description": "precedes under relation",
        "prefix": "\\prec ⊰",
        "scope": "pvs",
        "body": [ "⊰" ]
    }, {
        "description": "preceq",
        "prefix": "\\preceq ⪯",
        "scope": "pvs",
        "body": [ "⪯" ]
    }, {
        "description": "succeeds under relation",
        "prefix": "\\succ ⊱",
        "scope": "pvs",
        "body": [ "⊱" ]
    }, {
        "description": "succeq",
        "prefix": "\\succeq ⪰",
        "scope": "pvs",
        "body": [ "⪰" ]
    }, {
        "description": "member of",
        "prefix": "\\in ∈",
        "scope": "pvs",
        "body": [ "∈" ]
    }, {
        "description": "not a member of",
        "prefix": "\\notin ∉",
        "scope": "pvs",
        "body": [ "∉" ]
    }, {
        "description": "contains",
        "prefix": "\\ni ∋",
        "scope": "pvs",
        "body": [ "∋" ]
    }, {
        "description": "intersection",
        "prefix": "\\intersection ∩",
        "scope": "pvs",
        "body": [ "∩" ]
    }, {
        "description": "union",
        "prefix": "\\union ∪",
        "scope": "pvs",
        "body": [ "∪" ]
    }, {
        "description": "union",
        "prefix": "\\cup ∪",
        "scope": "pvs",
        "body": [ "∪" ]
    }, {
        "description": "subset of",
        "prefix": "\\subset ⊂",
        "scope": "pvs",
        "body": [ "⊂" ]
    }, {
        "description": "superset of",
        "prefix": "\\supset ⊃",
        "scope": "pvs",
        "body": [ "⊃" ]
    }, {
        "description": "not a subset of",
        "prefix": "\\nsubset ⊄",
        "scope": "pvs",
        "body": [ "⊄" ]
    }, {
        "description": "not a superset of",
        "prefix": "\\nsupset ⊅",
        "scope": "pvs",
        "body": [ "⊅" ]
    }, {
        "description": "subset of or equal to",
        "prefix": "\\subseteq ⊆",
        "scope": "pvs",
        "body": [ "⊆" ]
    }, {
        "description": "superser of or equal to",
        "prefix": "\\supseteq ⊇",
        "scope": "pvs",
        "body": [ "⊇" ]
    }, {
        "description": "subset of but not equal to",
        "prefix": "\\subsetneq ⊊",
        "scope": "pvs",
        "body": [ "⊊" ]
    }, {
        "description": "superset of but not equal to",
        "prefix": "\\supsetneq ⊋",
        "scope": "pvs",
        "body": [ "⊋" ]
    }, {
        "description": "multiset union",
        "prefix": "\\uplus ⊎",
        "scope": "pvs",
        "body": [ "⊎" ]
    }, {
        "description": "square image of",
        "prefix": "\\ltsquare ⊏",
        "scope": "pvs",
        "body": [ "⊏" ]
    }, {
        "description": "square original of",
        "prefix": "\\gtsquare ⊐",
        "scope": "pvs",
        "body": [ "⊐" ]
    }, {
        "description": "square image of or equal to",
        "prefix": "\\lesquare ⊑",
        "scope": "pvs",
        "body": [ "⊑" ]
    }, {
        "description": "square original or or equal to",
        "prefix": "\\gtsquare ⊒",
        "scope": "pvs",
        "body": [ "⊒" ]
    }, {
        "description": "square cap",
        "prefix": "\\sqcap ⊓",
        "scope": "pvs",
        "body": [ "⊓" ]
    }, {
        "description": "square cup",
        "prefix": "\\sqcup ⊔",
        "scope": "pvs",
        "body": [ "⊔" ]
    }, {
        "description": "n-ary logical AND",
        "prefix": "\\bigwedge ⋀",
        "scope": "pvs",
        "body": [ "⋀" ]
    }, {
        "description": "n-ary logical OR",
        "prefix": "\\bigvee ⋁",
        "scope": "pvs",
        "body": [ "⋁" ]
    }, {
        "description": "logical AND",
        "prefix": "\\and ∧",
        "scope": "pvs",
        "body": [ "∧" ]
    }, {
        "description": "logical OR",
        "prefix": "\\or ∨",
        "scope": "pvs",
        "body": [ "∨" ]
    },  {
        "description": "logical AND",
        "prefix": "\\wedge ∧",
        "scope": "pvs",
        "body": [ "∧" ]
    }, {
        "description": "logical OR",
        "prefix": "\\vee ∨",
        "scope": "pvs",
        "body": [ "∨" ]
    }, {
        "description": "bullet",
        "prefix": "\\bullet •",
        "scope": "pvs",
        "body": [ "•" ]
    }, {
        "description": "cdot",
        "prefix": "\\cdot ·",
        "scope": "pvs",
        "body": [ "·" ]
    }, {
        "description": "cdot",
        "prefix": "\\centerdot ·",
        "scope": "pvs",
        "body": [ "·" ]
    }, {
        "description": "left arrow",
        "prefix": "\\leftarrow ←",
        "scope": "pvs",
        "body": [ "←" ]
    }, {
        "description": "up arrow",
        "prefix": "\\uparrow ↑",
        "scope": "pvs",
        "body": [ "↑" ]
    }, {
        "description": "right arrow",
        "prefix": "\\rightarrow →",
        "scope": "pvs",
        "body": [ "→" ]
    }, {
        "description": "double right arrow",
        "prefix": "\\Rightarrow ⇒",
        "scope": "pvs",
        "body": [ "⇒" ]
    }, {
        "description": "down arrow",
        "prefix": "\\downarrow ↓",
        "scope": "pvs",
        "body": [ "↓" ]
    }, {
        "description": "right arrow squiggle",
        "prefix": "\\leadsto ↝",
        "scope": "pvs",
        "body": [ "↝" ]
    }, {
        "description": "right arrow from bar",
        "prefix": "\\mapsto ↦",
        "scope": "pvs",
        "body": [ "↦" ]
    }, {
        "description": "long right arrow from bar",
        "prefix": "\\longmapsto ⟼",
        "scope": "pvs",
        "body": [ "⟼" ]
    }, {
        "description": "long right arrow",
        "prefix": "\\longrightarrow ⟶",
        "scope": "pvs",
        "body": [ "⟶" ]
    }, {
        "description": "long left arrow",
        "prefix": "\\longleftarrow ⟵",
        "scope": "pvs",
        "body": [ "⟵" ]
    }, {
        "description": "long right double arrow",
        "prefix": "\\Longrightarrow ⟹",
        "scope": "pvs",
        "body": [ "⟹" ]
    }, {
        "description": "long left double arrow",
        "prefix": "\\Longleftarrow ⟸",
        "scope": "pvs",
        "body": [ "⟸" ]
    }, {
        "description": "dash right arrow",
        "prefix": "\\dashrightarrow ⇢",
        "scope": "pvs",
        "body": [ "⇢" ]
    }, {
        "description": "dash left arrow",
        "prefix": "\\dashleftarrow ⇠",
        "scope": "pvs",
        "body": [ "⇠" ]
    }, {
        "description": "rightharpoonup",
        "prefix": "\\rightharpoonup ⇀",
        "scope": "pvs",
        "body": [ "⇀" ]
    }, {
        "description": "leftharpoonup",
        "prefix": "\\leftharpoonup ↼",
        "scope": "pvs",
        "body": [ "↼" ]
    }, {
        "description": "rightharpoondown",
        "prefix": "\\rightharpoondown ⇁",
        "scope": "pvs",
        "body": [ "⇁" ]
    }, {
        "description": "leftharpoondown",
        "prefix": "\\leftharpoondown ↽",
        "scope": "pvs",
        "body": [ "↽" ]
    }, {
        "description": "hook left arrow",
        "prefix": "\\hookleftarrow ↩",
        "scope": "pvs",
        "body": [ "↩" ]
    }, {
        "description": "hook right arrow",
        "prefix": "\\hookrightarrow ↪",
        "scope": "pvs",
        "body": [ "↪" ]
    }, {
        "description": "double left arrow",
        "prefix": "\\Leftarrow ⇐",
        "scope": "pvs",
        "body": [ "⇐" ]
    }, {
        "description": "double up arrow",
        "prefix": "\\Uparrow ⇑",
        "scope": "pvs",
        "body": [ "⇑" ]
    }, {
        "description": "double down arrow",
        "prefix": "\\Downarrow ⇓",
        "scope": "pvs",
        "body": [ "⇓" ]
    }, {
        "description": "triangle down",
        "prefix": "\\triangledown ∇",
        "scope": "pvs",
        "body": [ "∇" ]
    },  {
        "description": "triangle down",
        "prefix": "\\nabla ∇",
        "scope": "pvs",
        "body": [ "∇" ]
    }, {
        "description": "turnstile left",
        "prefix": "\\turnstileleft ⊣",
        "scope": "pvs",
        "body": [ "⊣" ]
    }, {
        "description": "turnstile left",
        "prefix": "\\dashv ⊣",
        "scope": "pvs",
        "body": [ "⊣" ]
    }, {
        "description": "bottom",
        "prefix": "\\bot ⊥",
        "scope": "pvs",
        "body": [ "⊥" ]
    }, {
        "description": "bar turnstile",
        "prefix": "\\barturnstile ⊩",
        "scope": "pvs",
        "body": [ "⊩" ]
    }, {
        "description": "bar turnstile",
        "prefix": "\\VDash ⊩",
        "scope": "pvs",
        "body": [ "⊩" ]
    }, {
        "description": "not bar turnstile",
        "prefix": "\\nVDash ⊮",
        "scope": "pvs",
        "body": [ "⊮" ]
    }, {
        "description": "big circle",
        "prefix": "\\bigcirc ◯",
        "scope": "pvs",
        "body": [ "◯" ]
    }, {
        "description": "star",
        "prefix": "\\star ★",
        "scope": "pvs",
        "body": [ "★" ]
    }, {
        "description": "maltese cross",
        "prefix": "\\maltese ✠",
        "scope": "pvs",
        "body": [ "✠" ]
    }, {
        "description": "box",
        "prefix": "\\Box □",
        "scope": "pvs",
        "body": [ "□" ]
    }, {
        "description": "box",
        "prefix": "\\square □",
        "scope": "pvs",
        "body": [ "□" ]
    }, {
        "description": "diamond",
        "prefix": "\\Diamond ◇",
        "scope": "pvs",
        "body": [ "◇" ]
    }, {
        "description": "angular bracket left",
        "prefix": "\\langle 〈",
        "scope": "pvs",
        "body": [ "〈" ]
    }, {
        "description": "angular bracket right",
        "prefix": "\\rnagle 〉",
        "scope": "pvs",
        "body": [ "〉" ]
    }, {
        "description": "left ceiling",
        "prefix": "\\lceil ⌈",
        "scope": "pvs",
        "body": [ "⌈" ]
    }, {
        "description": "right ceiling",
        "prefix": "\\rceil ⌉",
        "scope": "pvs",
        "body": [ "⌉" ]
    }, {
        "description": "left floor",
        "prefix": "\\lfloor ⌊",
        "scope": "pvs",
        "body": [ "⌊" ]
    }, {
        "description": "right floor",
        "prefix": "\\rfloor ⌋",
        "scope": "pvs",
        "body": [ "⌋" ]
    }, {
        "description": "lambda",
        "prefix": "\\lambda λ",
        "scope": "pvs",
        "body": [ "λ" ]
    }, {
        "description": "up left corner",
        "prefix": "\\ulcorner ⌜",
        "scope": "pvs",
        "body": [ "⌜" ]
    }, {
        "description": "up right corner",
        "prefix": "\\urcorner ⌝",
        "scope": "pvs",
        "body": [ "⌝" ]
    }, {
        "description": "double square bracket left",
        "prefix": "\\llbracket ⟦",
        "scope": "pvs",
        "body": [ "⟦" ]
    }, {
        "description": "double square bracket right",
        "prefix": "\\rrbracket ⟧",
        "scope": "pvs",
        "body": [ "⟧" ]
    }, {
        "description": "lower left corner",
        "prefix": "\\llcorner ⌞",
        "scope": "pvs",
        "body": [ "⌞" ]
    }, {
        "description": "lower right corner",
        "prefix": "\\lrcorner ⌟",
        "scope": "pvs",
        "body": [ "⌟" ]
    }, {
        "description": "double angular bracket left",
        "prefix": "\\ldata ⟪",
        "scope": "pvs",
        "body": [ "⟪" ]
    }, {
        "description": "double angular bracket right",
        "prefix": "\\rdata ⟫",
        "scope": "pvs",
        "body": [ "⟫" ]
    }, {
        "description": "section sign",
        "prefix": "\\section §",
        "scope": "pvs",
        "body": [ "§" ]
    }, {
        "description": "nmid",
        "prefix": "\\nmid ∤",
        "scope": "pvs",
        "body": [ "∤" ]
    }, {
        "description": "wr",
        "prefix": "\\wr ≀",
        "scope": "pvs",
        "body": [ "≀" ]
    }
];
/**
 * Utility function, returns symbols as an array suitable to be used for rendering autocomplete tooltips
 */
export function getMathSymbols (): string[] {
    return symbols.map((sym: Symbol) => {
        return sym.prefix;
    }) || [];
};