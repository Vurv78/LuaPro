local Lexer = require("compiler/lexer/lua")
local Parser = require("compiler/parser/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
	local X = "Hello"
	Y = "World!"

	-- Comment
	hello(55)

	::myl_abel::
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

assert(nodes[3] and nodes[3].kind == Parser.Kinds.Comment, "Expected comment")

Assert.equal(nodes[3].data[2], " Comment")

assert(nodes[4] and nodes[4].kind == Parser.Kinds.Call, "Expected call")

Assert.equal(nodes[4].data[1].data[1], "hello")

-- check first parameter
Assert.equal(#nodes[4].data[2], 1) -- should only have 1 param
Assert.equal(nodes[4].data[2][1].kind, Parser.Kinds.Literal) -- should be a literal
Assert.equal(nodes[4].data[2][1].data[3], 55) -- number value

assert(nodes[5] and nodes[5].kind == Parser.Kinds.Label, "Expected label")
Assert.equal(nodes[5].data[1], "myl_abel")