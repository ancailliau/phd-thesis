local format, tohash = string.format, table.tohash
local P, S, V, R, patterns = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.patterns

local context                 = context
local verbatim                = context.verbatim
local makepattern             = visualizers.makepattern

local KTSnippet              = context.KTSnippet
local startKTSnippet         = context.startKTSnippet
local stopKTSnippet          = context.stopKTSnippet

local KTSnippetBoundary      = verbatim.KTSnippetBoundary
local KTSnippetQuote         = verbatim.KTSnippetQuote
local KTSnippetString        = verbatim.KTSnippetString
local KTSnippetSpecial       = verbatim.KTSnippetSpecial
local KTSnippetComment       = verbatim.KTSnippetComment
local KTSnippetOpenKeyword   = verbatim.KTSnippetOpenKeyword
local KTSnippetNameKeyword   = verbatim.KTSnippetNameKeyword
local KTSnippetName          = verbatim.KTSnippetName
local KTSnippetNumber        = verbatim.KTSnippetNumber

local keywords = tohash {
   "is", "rsr", "probability", "exception",
   "assumption", "argument", "description", "refinedby", "attribute", "link",
   "obstructedby", "resolvedby", "definition", "name", "import", "id",
   "assignedto", "formalspec", "signature", "esr", "agenttype", "entitytype", "isa", "type"
}

local open_keywords = tohash {
   "declare", "override", "end", "import"
}

local function visualizename(s)
    if open_keywords[s] then
        KTSnippetOpenKeyword(s)
    elseif keywords[s] then
        KTSnippetNameKeyword(s)
    else
        KTSnippetName(s)
    end
end

local handler = visualizers.newhandler {
    startinline  = function() KTSnippet(false,"{") end,
    stopinline   = function() context("}") end,

    startdisplay = function() startKTSnippet() end,
    stopdisplay  = function() stopKTSnippet() end ,

    boundary     = function(s) KTSnippetBoundary(s) end,
    special      = function(s) KTSnippetSpecial(s) end,
    comment      = function(s) KTSnippetComment(s) end,
    quote        = function(s) KTSnippetQuote(s) end,
    string       = function(s) KTSnippetString(s) end,
    
    number       = function(s) KTSnippetNumber(s) end,
    
    custom_attr  = function(s) KTSnippetNameKeyword(s) end,
    model_attr   = function(s) KTSnippetOpenKeyword(s) end,

    name         = visualizename,
}

local comment     = P '#' * (1 - S '\r\n\f') ^ 0
local name        = (patterns.letter + S("_"))^1
local boundary    = S('()[]:=<>;"')
local special     = S("-+/*|`!?^&%.,")
local custom_attr = P '$' * ( R("az", "AZ") + P('_') ) ^ 1
local model_attr  = P '@' * ( R("az", "AZ") + S('_.') ) ^ 1

local number = {}

local digit = R("09")

-- Matches: 10, -10, 0
number.integer =
	(S("+-") ^ -1) *
	(digit   ^  1)

-- Matches: .6, .899, .9999873
number.fractional =
	(P(".")   ) *
	(digit ^ 1)

-- Matches: 55.97, -90.8, .9 
number.decimal =	
	(number.integer *              -- Integer
	(number.fractional ^ -1)) +    -- Fractional
	(S("+-") * number.fractional)  -- Completely fractional number

-- Matches: 60.9e07, 9e-4, 681E09 
number.scientific = 
	number.decimal * -- Decimal number
	S("Ee") *        -- E or e
	number.integer   -- Exponent

-- Matches all of the above
number.number =
	number.decimal + number.scientific -- Decimal number allows for everything else, and scientific matches scientific

local grammar = visualizers.newgrammar("default", { "visualizer",

    comment     = makepattern(handler,"comment",comment),
    dstring     = makepattern(handler,"quote",patterns.dquote)
                * makepattern(handler,"string",patterns.nodquote)
                * makepattern(handler,"quote",patterns.dquote),
    name        = makepattern(handler,"name",name),
    custom_attr = makepattern(handler,"custom_attr",custom_attr),
    model_attr  = makepattern(handler,"model_attr",model_attr),
    boundary    = makepattern(handler,"boundary",boundary),
    special     = makepattern(handler,"special",special),
    number      = makepattern(handler,"number",number.number),

    pattern     =
        V("comment") + V("dstring") + V("name") + V("boundary") + V("special")
      + V("number") + V("custom_attr") + V("model_attr")
      + V("newline") * V("emptyline")^0 * V("beginline")
      + V("space")
      + V("default"),

    visualizer  =
        V("pattern")^1

} )

local parser = P(grammar)

visualizers.register("kt", { parser = parser, handler = handler, grammar = grammar } )