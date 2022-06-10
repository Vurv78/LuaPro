---@type Lexer
local Lexer = require("compiler/lexer/lua")

---@type Parser
local Parser = require("compiler/parser/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
	local X = "Hello"
	Y = "World!"

	-- Comment
]])

assert(tokens, "Failed to parse tokens")

local parser = Parser.new()
local nodes = parser:parse(tokens)

assert(nodes[1].kind == Parser.Kinds.LVarDecl)
assert(#nodes[1].data[1] == 1, "Expected only 1 name in list of names for local declaration")
assert(nodes[1].data[1][1] == "X" and nodes[1].data[2][1].kind == Parser.Kinds.Literal, "Expected X to be a literal")

assert(nodes[2].kind == Parser.Kinds.VarAssign)
assert(#nodes[2].data[1] == 1, "Expected only 1 variable in assignment")
assert(nodes[2].data[1][1] == "Y" and nodes[1].data[2][1].kind == Parser.Kinds.Literal, "Expected Y to be a literal")
assert(nodes[2].data[2][1].data[1] == "string" and nodes[2].data[2][1].data[3] == "World!", "Expected value of literal to be 'World!'" )