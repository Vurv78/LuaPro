local lib = require("src.lib.mod")
local tokenize, parse = lib.tokenize, lib.parse

it("should format the readme as expected", function()
	local handle = assert( io.open("README.md", "r"), "Could not open README.md" )
	local markdown = handle:read("*a")
	handle:close()

	local sources = {}
	for src in markdown:gmatch("```lua([^`]+)```") do
		sources[#sources + 1] = src:sub(2, -2):gsub("\r", "")
	end

	-- expect(sources[2]).toBe( parse(tokenize(sources[1])):display() )
end)