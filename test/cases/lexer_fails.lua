local Lexer = require("lexer/lua")

local lexer = Lexer.new()

local LEX_FAILS = {
	["::label+::"] = ":: expected near '+'",
	-- ["0b"] = "malformed number near '0b'"
}

for v, msg in pairs(LEX_FAILS) do
	lexer:reset()

	local ok = pcall(lexer.parse, lexer, v)
	assert(not ok, "Expected lexing ['" .. v .. "'] to fail: " .. msg)
end