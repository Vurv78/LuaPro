package.path = package.path .. ";src/?.lua"

---@type Lexer
local Lexer = require("compiler/lexer/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
	local X = "Hello"
	Y = "World!"
]])


assert(tokens[1].kind == Lexer.Kinds.Keyword and tokens[1].raw == "local")
assert(tokens[2].kind == Lexer.Kinds.Identifier and tokens[2].raw == "X")
assert(tokens[3].kind == Lexer.Kinds.Operator and tokens[3].raw == "=")
assert(tokens[4].kind == Lexer.Kinds.String and tokens[4].raw == "\"Hello\"")
assert(tokens[5].kind == Lexer.Kinds.Identifier and tokens[5].raw == "Y")
assert(tokens[6].kind == Lexer.Kinds.Operator and tokens[6].raw == "=")
assert(tokens[7].kind == Lexer.Kinds.String and tokens[7].raw == "\"World!\"")