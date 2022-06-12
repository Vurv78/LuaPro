local Lexer = require("compiler/lexer/lua")
local Parser = require("compiler/parser/lua")
local Transpiler = require("compiler/codegen/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
local var                               = 55
xyz = xyz

::test::
goto test
]])

assert(tokens, "Failed to parse tokens")

local parser = Parser.new()
local nodes = parser:parse(tokens)
assert(nodes, "Failed to generate AST")

local transpiler = Transpiler.new( require("compiler/codegen/mode-lua/format") )
local code = transpiler:process(nodes)

Assert.equal(code, "local var = 55\nxyz = xyz\n::test::\ngoto test")