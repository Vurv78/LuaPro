local Lexer = require("compiler/lexer")
local Parser = require("compiler/parser")
local Transpiler = require("compiler/transpiler")

local handle = io.open("../in.lua", "rb")
local src = handle:read("*a")

local lexer = Lexer.new()
local tokens = lexer:parse(src)

for k, v in ipairs(tokens) do
	print("Token", k, v.val)
end

local parser = Parser.new()
local nodes = parser:parse(tokens)

for k, v in ipairs(nodes) do
	print("Node", k, v)
end

local transpiler = Transpiler.new()
local code = transpiler:process(nodes)

local handle = io.open("../out.lua", "wb")
handle:write(code)
handle:close()