local Lexer = require("compiler/lexer/lua")
local Parser = require("compiler/parser/lua")

local lexer = Lexer.new()

local PARSE_FAILS = {
	["while;false;do;end"] = "Expected 'end'",
	["test"] = "Can't have an expression at the top level",
	["end"] = "<eof> expected near 'end'",
	["local = 5"] = "<name> expected near '='"
}

local parser = Parser.new()
for v, msg in pairs(PARSE_FAILS) do
	lexer:reset()
	parser:reset()

	local tokens = lexer:parse(v)
	local ok = pcall(parser.parse, parser, tokens)
	assert(not ok, "Expected parse ['" .. v .. "'] to fail: " .. msg)
end