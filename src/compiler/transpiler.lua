local NODE_KINDS = require("compiler/parser").Kinds

---@class Transpiler
local Transpiler = {}
Transpiler.__index = Transpiler

function Transpiler.new()
	return setmetatable({}, Transpiler)
end

local fmt = string.format

local Transpilers = {
	---@param self Transpiler
	---@param data table
	[NODE_KINDS.While] = function(self, data)
		local cond = self:transpile( data[1] )
		local body = self:transpile( data[2] )
		return fmt("while %s do\n%s\nend", cond, body)
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Function] = function(self, data)
		local name, args, block = data[1], data[2], data[3]
		return fmt("function %s(%s)\n\t%s\nend", name, table.concat(args, ", "), self:transpileAst(block))
	end,

	[NODE_KINDS.LVarDecl] = function(self, data)
		local names, vals = data[1], data[2]

		if vals then
			local valstrs = {}
			for i = 1, #vals do
				print("vs", vals[i])
				valstrs[i] = self:transpile(vals[i])
			end

			return fmt("local %s = %s", table.concat(names, ", "), table.concat(valstrs, ", "))
		else
			return fmt("local %s", table.concat(names, ", "))
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Literal] = function(self, data)
		local kind, raw, val = data[1], data[2], data[3]
		if kind == "string" then
			return fmt("%q", val)
		elseif kind == "number" then
			return tostring(val)
		else
			return raw
		end
	end
}

---@param node Node
---@return string
function Transpiler:transpile(node)
	local handler = Transpilers[node.kind]
	if handler then
		return handler(self, node.data)
	end

	print("Unimplemented Transpile target: ", node.kind)
	return ""
end

---@param ast table<number, Node>
---@param indent boolean
---@return string
function Transpiler:transpileAst(ast, indent)
	local ret = {}
	if not ast then return "" end -- Empty block

	self.nodes = ast
	for i, node in ipairs(ast) do
		self.current = i
		if indent then
			ret[i] = string.gsub(self:transpile(node), "\n", "\n\t")
		else
			ret[i] = self:transpile(node)
		end
	end
	return table.concat(ret, indent and "\n\t" or "\n")
end

--- Transpiles Lua into.. better lua.
--- Get the ast from the [Analyzer].
---@param ast table<number, Node> # AST retrieved from the [Analyzer] or [Parser]
---@return string
function Transpiler:process(ast)
	assert(type(ast) == "table", "bad argument #1 to 'Transpiler:process' (table expected, got " .. type(ast) .. ")")

	self.ast = ast

	return self:transpileAst(ast)
end

return Transpiler