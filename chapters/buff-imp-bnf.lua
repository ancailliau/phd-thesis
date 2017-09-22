local format, tohash = string.format, table.tohash
local P, S, V, R, patterns = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.patterns

local context                 = context
local verbatim                = context.verbatim
local makepattern             = visualizers.makepattern

local BNFSnippet              = context.BNFSnippet
local startBNFSnippet         = context.startBNFSnippet
local stopBNFSnippet          = context.stopBNFSnippet

local BNFSnippetNonTerminal   = verbatim.BNFSnippetNonTerminal
local BNFSnippetTerminal      = verbatim.BNFSnippetTerminal
local BNFSnippetComment       = verbatim.BNFSnippetComment

local handler = visualizers.newhandler {
    startinline  = function() BNFSnippet(false,"{") end,
    stopinline   = function() context("}") end,

    startdisplay = function() startBNFSnippet() end,
    stopdisplay  = function() stopBNFSnippet() end ,

    nonterminal  = function(s) BNFSnippetNonTerminal(s) end,
    terminal     = function(s) BNFSnippetTerminal(s) end,
    quote        = function(s) BNFSnippetTerminal(s) end,
    comment      = function(s) BNFSnippetComment(s) end,
}

local nonterminal    = P '<' * (1 - S '>') ^ 0 * P '>'
local comment     = P '#' * (1 - S '\r\n\f') ^ 0

local grammar = visualizers.newgrammar("default", { "visualizer",

    comment     = makepattern(handler,"comment",comment),
    nonterminal = makepattern(handler,"nonterminal",nonterminal),
    terminal    = makepattern(handler,"quote",patterns.dquote)
                * makepattern(handler,"terminal",patterns.nodquote)
                * makepattern(handler,"quote",patterns.dquote),

    pattern     =
        V("comment") + V("nonterminal") + V("terminal") 
      + V("newline") * V("emptyline")^0 * V("beginline")
      + V("space")
      + V("default"),

    visualizer  =
        V("pattern")^1

} )

local parser = P(grammar)

visualizers.register("bnf", { parser = parser, handler = handler, grammar = grammar } )