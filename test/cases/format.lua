---@type Lexer
local Lexer = require("compiler/lexer/lua")

---@type Parser
local Parser = require("compiler/parser/lua")

---@type Transpiler
local Transpiler = require("compiler/codegen/lua")

local lexer = Lexer.new()
local tokens = lexer:parse([[
local var                               = 55
xyz
]])

assert(tokens, "Failed to parse tokens")

local parser = Parser.new()
local nodes = parser:parse(tokens)
assert(nodes, "Failed to generate AST")

local transpiler = Transpiler.new( require("compiler/codegen/mode-lua/format") )
local code = transpiler:process(nodes)

assert(code == "local var = 55\nxyz", "Code was improperly formatted. Expected 'local var = 55', got '" .. code .. "'")