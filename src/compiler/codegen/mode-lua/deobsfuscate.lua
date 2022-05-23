---@type NodeKinds
local NODE_KINDS = require("compiler/parser/lua").Kinds

local fmt = string.format

-- Extends format mode
local FormatMode = assert( require("compiler/codegen/mode-lua/format") )

---@param self Transpiler
---@param data table
FormatMode[NODE_KINDS.Comment] = function(self, data)
	local multiline, inner, depth = data[1], data[2], data[3]

	if multiline then
		local equals = string.rep("=", depth)
		return fmt("--[%s[%s]%s]", equals, inner, equals)
	else
		return "--" .. inner
	end
end

return FormatMode