--[[
	Config
]]

local CYCLES = 20000
local MAX_SECONDS_ELAPSED = jit and 1 or 5 -- Allow non-jit versions to be 5x as slow.

-- 180,000 LOC total (20k * 9 lines)
local SRC = [[
	local X = "Hello"
	Y = "World!"

	-- Comment
	hello(55)

	::myl_abel::

	goto myl_abel
]]

--[[
	End Config
]]

local Lexer = require("lexer/lua")
local Parser = require("parser/lua")

local lexer, parser = Lexer.new(), Parser.new()

local before = os.time()

for _ = 1, CYCLES do
	lexer:reset()
	parser:reset()

	local tokens = lexer:parse(SRC)

	assert(tokens, "Failed to parse tokens")

	local ast = parser:parse(tokens)
	assert(ast, "Failed to parse AST")
end

if os.time() - before > MAX_SECONDS_ELAPSED then
	local function comma(num)
		num = tostring(num)

		local index = -1
		while index ~= 0 do
			num, index = num:gsub( "^(-?%d+)(%d%d%d)", "%1,%2" )
		end

		return num
	end

	error("Took too long to parse " .. comma(CYCLES * #SRC) .. " loc")
end