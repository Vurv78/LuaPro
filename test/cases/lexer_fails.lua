local Lexer = require("compiler/lexer/lua")

local lexer = Lexer.new()

local LEX_FAILS = {
	["::label+::"] = ":: expected near '+'",
}

for v, msg in pairs(LEX_FAILS) do
	lexer:reset()

	local ok = pcall(lexer.parse, lexer, v)
	assert(not ok, "Expected lexing ['" .. v .. "'] to fail: " .. msg)
end