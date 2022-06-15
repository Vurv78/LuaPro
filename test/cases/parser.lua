local Lexer = require("lexer/lua")
local Parser = require("parser/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
	local X = "Hello"
	Y = "World!"

	-- Comment
	hello(55)

	::myl_abel::
	goto myl_abel

	print "test"
	print { "xyz", 52 }
]])

assert(tokens, "Failed to parse tokens")

local parser = Parser.new()
local nodes = parser:parse(tokens)

assert(nodes[1].kind == Parser.Kinds.LVarDecl)
assert(#nodes[1].data[1] == 1, "Expected only 1 name in list of names for local declaration")
assert(nodes[1].data[1][1] == "X" and nodes[1].data[2][1].kind == Parser.Kinds.Literal, "Expected X to be a literal")

assert(nodes[2].kind == Parser.Kinds.VarAssign)
assert(#nodes[2].data[1] == 1, "Expected only 1 variable in assignment")

-- Assignment contains a list of identifiers OR index ops
Assert.equal(nodes[2].data[1][1].kind, Parser.Kinds.Identifier)
Assert.equal(nodes[2].data[2][1].kind, Parser.Kinds.Literal)

-- nodes[2] -> 2nd node (Y = "World!")
-- .data[2] -> list of values
-- [1] -> 1st value ("World!") (As a literal node)
-- .data[1] -> Literal type
Assert.equal(nodes[2].data[2][1].data[1], "string")
Assert.equal(nodes[2].data[2][1].data[3], "World!")

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

assert(nodes[6] and nodes[6].kind == Parser.Kinds.Escape, "Expected escape")
Assert.equal(nodes[6].data[1], "goto") -- [1] is the escape type (goto|break|return)
Assert.equal(nodes[6].data[2], "myl_abel") -- [2] is the label name

Assert.equal(nodes[7].kind, Parser.Kinds.Call)
Assert.equal(nodes[7].data[1].kind, Parser.Kinds.Identifier)
Assert.equal(nodes[7].data[1].data[1], "print")

Assert.equal(nodes[7].data[2][1].kind, Parser.Kinds.Literal)
Assert.equal(nodes[7].data[2][1].data[1], "string")
Assert.equal(nodes[7].data[2][1].data[3], "test")

Assert.equal(nodes[8].kind, Parser.Kinds.Call)
Assert.equal(nodes[8].data[1].kind, Parser.Kinds.Identifier)
Assert.equal(nodes[8].data[1].data[1], "print")

Assert.equal(nodes[8].data[2][1].kind, Parser.Kinds.Table)
Assert.equal(#nodes[8].data[2][1].data[1], 2) -- 2 elements in table
