--[[
	This is a test to make sure the README.md example still actually parses and compiles properly.
]]


local handle = assert( io.open("README.md", "r"), "Could not open README.md" )
local markdown = handle:read("*a")
handle:close()

local sources = {}
for src in markdown:gmatch("```lua([^`]+)```") do
	sources[#sources + 1] = src:sub(2, -2):gsub("\r", "")
end

local lexer = require("lexer/lua").new()
local parser = require("parser/lua").new()
local transpiler = require("codegen/lua").new(require("codegen/mode-lua/deobfuscate"))

local tokens = assert( lexer:parse(sources[1]) )
local nodes = assert( parser:parse(tokens) )
local code = assert( transpiler:process(nodes) )

Assert.equal(sources[2], code)