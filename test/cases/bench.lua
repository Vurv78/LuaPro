--[[
	Config
]]

-- Try to parse ~10kb of lua source code.
-- Source code tries to cover most lua features for all regressions.
local CYCLES = 30000
local MAX_SECONDS_ELAPSED = jit and 4 or 12 -- Allow non-jit versions to be 3x as slow.

local SRC = [==[
	local X = "Hello"
	Y = 55.23

	-- Comment
	--[[
		Multi-line comment
	]]
	world(Y)

	::myl_abel::
	goto myl_abel

	local Z = {
		[55] = 2,
		"test"
	}
	function Z.yz(a, b, c)
		repeat
		until true

		while true do end
	end

	local abcdefghijklmnopqrstuvwxyz = [[abcdefghijklmnopqrstuvwxyz]]

	for _ = 1, 2, 3 do
		return function(lambdafn) end
	end
]==]

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

local function comma(num)
	num = tostring(num)

	local index = -1
	while index ~= 0 do
		num, index = num:gsub( "^(-?%d+)(%d%d%d)", "%1,%2" )
	end

	return num
end

local UNITS = {"B", "KB", "MB", "GB", "TB"}
local function format_bytes(bytes)
	local unit = 0

	while bytes > 1024 do
		bytes = bytes / 1024

		if unit > 4 then
			break
		end

		unit = unit + 1
	end

	return string.format("%.2f %s", bytes, UNITS[unit])
end

local formatted_bytes = comma( format_bytes(CYCLES * #SRC) )

local elapsed = os.time() - before
if elapsed > MAX_SECONDS_ELAPSED then
	error("Took too long to parse " .. formatted_bytes .. ": took " .. comma(elapsed) .. " seconds")
end

-- print() is overridden in unit tests (for debugging errors)
-- use io.write to bypass
io.write("Parsed " .. formatted_bytes .. " in " .. comma(elapsed) .. " seconds\n")