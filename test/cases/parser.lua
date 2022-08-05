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

	local Z = -55
	local Tralse = not not false
]])

assert(tokens, "Failed to parse tokens")

local parser = Parser.new()
local nodes = parser:parse(tokens)

do -- local X = "Hello"
	local stmt = nodes[1]

	Assert.equal(stmt.kind, Parser.Kinds.LVarDecl)
	assert(#stmt.data[1] == 1, "Expected only 1 name in list of names for local declaration")
	assert(stmt.data[1][1] == "X" and stmt.data[2][1].kind == Parser.Kinds.Literal, "Expected X to be a literal")
end

do -- Y = "World!"
	local stmt = nodes[2]

	assert(stmt.kind == Parser.Kinds.VarAssign)
	assert(#stmt.data[1] == 1, "Expected only 1 variable in assignment")

	-- Assignment contains a list of identifiers OR index ops
	Assert.equal(stmt.data[1][1].kind, Parser.Kinds.Identifier)
	Assert.equal(stmt.data[2][1].kind, Parser.Kinds.Literal)

	-- stmt -> 2nd node (Y = "World!")
	-- .data[2] -> list of values
	-- [1] -> 1st value ("World!") (As a literal node)
	-- .data[1] -> Literal type
	Assert.equal(stmt.data[2][1].data[1], "string")
	Assert.equal(stmt.data[2][1].data[3], "World!")
end

do -- Comment
	local stmt = nodes[3]

	assert(stmt and stmt.kind == Parser.Kinds.Comment, "Expected comment")

	Assert.equal(stmt.data[2], " Comment")
end

do -- hello(55)
	local stmt = nodes[4]

	assert(stmt and stmt.kind == Parser.Kinds.Call, "Expected call")

	Assert.equal(stmt.data[1].data[1], "hello")

	-- check first parameter
	Assert.equal(#stmt.data[2], 1) -- should only have 1 param
	Assert.equal(stmt.data[2][1].kind, Parser.Kinds.Literal) -- should be a literal
	Assert.equal(stmt.data[2][1].data[3], 55) -- number value
end

do -- ::myl_abel::
	local stmt = nodes[5]

	assert(stmt and stmt.kind == Parser.Kinds.Label, "Expected label")
	Assert.equal(stmt.data[1], "myl_abel")
end

do -- goto myl_abel
	local stmt = nodes[6]

	assert(stmt and stmt.kind == Parser.Kinds.Escape, "Expected escape")
	Assert.equal(stmt.data[1], "goto") -- [1] is the escape type (goto|break|return)
	Assert.equal(stmt.data[2], "myl_abel") -- [2] is the label name
end

do -- print "test"
	local stmt = nodes[7]

	Assert.equal(stmt.kind, Parser.Kinds.Call)
	Assert.equal(stmt.data[1].kind, Parser.Kinds.Identifier)
	Assert.equal(stmt.data[1].data[1], "print")

	Assert.equal(stmt.data[2][1].kind, Parser.Kinds.Literal)
	Assert.equal(stmt.data[2][1].data[1], "string")
	Assert.equal(stmt.data[2][1].data[3], "test")
end

do -- print { "xyz", 52 }
	local stmt = nodes[8]

	Assert.equal(stmt.kind, Parser.Kinds.Call)
	Assert.equal(stmt.data[1].kind, Parser.Kinds.Identifier)
	Assert.equal(stmt.data[1].data[1], "print")

	Assert.equal(stmt.data[2][1].kind, Parser.Kinds.Table)
	Assert.equal(#stmt.data[2][1].data[1], 2) -- 2 elements in table
end

do -- local Z = -55
	local stmt = nodes[9]

	Assert.equal(stmt.kind, Parser.Kinds.LVarDecl)
	Assert.equal(#stmt.data[1], 1) -- Only 1 name in local decl
	Assert.equal(stmt.data[1][1], "Z")

	local value = stmt.data[2][1]
	Assert.equal(value.kind, Parser.Kinds.UnaryOps)
	Assert.equal(value.data[1], "-")

	Assert.equal(value.data[2].kind, Parser.Kinds.Literal)
	Assert.equal(value.data[2].data[1], "number")
end

do -- local Tralse = not not false
	local stmt = nodes[10]

	Assert.equal(stmt.kind, Parser.Kinds.LVarDecl)
	Assert.equal(#stmt.data[1], 1) -- Only 1 name in local decl
	Assert.equal(stmt.data[1][1], "Tralse")

	local value = stmt.data[2][1]
	Assert.equal(value.kind, Parser.Kinds.UnaryOps)
	Assert.equal(value.data[1], "not")

	Assert.equal(value.data[2].kind, Parser.Kinds.UnaryOps)
	Assert.equal(value.data[2].data[1], "not")

	Assert.equal(value.data[2].data[2].kind, Parser.Kinds.Literal)
	Assert.equal(value.data[2].data[2].data[1], "boolean")
	Assert.equal(value.data[2].data[2].data[3], false)
end