---@type NodeKinds
local NODE_KINDS = require("parser/lua").Kinds

local fmt = string.format

-- Extends format mode
local Mode = assert( require("codegen/mode-lua/format") )

---@param self Transpiler
---@param data table
Mode[NODE_KINDS.Comment] = function(self, data)
	local multiline, inner, depth = data[1], data[2], data[3]

	if multiline then
		local equals = string.rep("=", depth)
		return fmt("--[%s[%s]%s]", equals, inner, equals)
	else
		return "--" .. inner
	end
end

---@param self Transpiler
---@param data table
Mode[NODE_KINDS.Literal] = function(self, data)
	local kind, raw, val = data[1], data[2], data[3]
	if kind == "string" then
		-- Replace escape sequences with their actual characters
		local str = val:gsub("\\x(%x%x)",function (x) return string.char(tonumber(x,16)) end)
		return fmt("%q", str)
	elseif kind == "number" then
		return tostring(val)
	else
		return raw
	end
end

return Mode