local Lexer = require("lexer/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
	local X = "Hello"
	Y = "World!"

	12 0x12 12.0

	::label::
]])


assert(tokens[1].kind == Lexer.Kinds.Keyword and tokens[1].raw == "local")
assert(tokens[2].kind == Lexer.Kinds.Identifier and tokens[2].raw == "X")
assert(tokens[3].kind == Lexer.Kinds.Operator and tokens[3].raw == "=")
assert(tokens[4].kind == Lexer.Kinds.String and tokens[4].raw == "\"Hello\"")
assert(tokens[5].kind == Lexer.Kinds.Identifier and tokens[5].raw == "Y")
assert(tokens[6].kind == Lexer.Kinds.Operator and tokens[6].raw == "=")
assert(tokens[7].kind == Lexer.Kinds.String and tokens[7].raw == "\"World!\"")

assert(tokens[8].kind == Lexer.Kinds.Integer and tokens[8].value == 12)
assert(tokens[9].kind == Lexer.Kinds.Hexadecimal and tokens[9].value == 0x12)
assert(tokens[10].kind == Lexer.Kinds.Decimal and tokens[10].value == 12.0)

assert(tokens[11].kind == Lexer.Kinds.Label and tokens[11].raw == "::label::")