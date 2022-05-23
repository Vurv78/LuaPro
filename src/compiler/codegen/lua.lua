---@class Transpiler
---@field nodes table<integer, Node>
---@field current integer
---@field transpilers table<number, fun(self: Transpiler, data: table<number, any>)>
local Transpiler = {}
Transpiler.__index = Transpiler

---@param mode table<number, fun(self: Transpiler, data: table<number, any>)>
function Transpiler.new(mode)
	return setmetatable({ transpilers = assert(mode, "Missing transpiler mode") }, Transpiler)
end

---@return Node?
function Transpiler:peek()
	return self.nodes[self.current + 1]
end

---@param node Node
---@return string
function Transpiler:transpile(node)
	local handler = self.transpilers[node.kind]
	if handler then
		return handler(self, node.data)
	end

	print("Unimplemented Transpile target: ", node.kind)
	return ""
end

---@param ast table<number, Node>
---@param indent boolean? Indent? Default true
---@return string
function Transpiler:transpileAst(ast, indent)
	local ret = {}
	if not ast then return "" end -- Empty block

	if indent == nil then indent = true end

	self.nodes = ast
	for i, node in ipairs(ast) do
		self.current = i
		if indent then
			ret[i] = string.gsub(self:transpile(node), "\n", "\n\t")
		else
			ret[i] = self:transpile(node)
		end
	end

	if indent then
		return "\t" .. table.concat(ret, "\n\t")
	else
		return table.concat(ret, "\n")
	end
end

--- Transpiles Lua into.. better lua.
--- Get the ast from the [Analyzer].
---@param ast table<number, Node> # AST retrieved from the [Analyzer] or [Parser]
---@return string
function Transpiler:process(ast)
	assert(type(ast) == "table", "bad argument #1 to 'Transpiler:process' (table expected, got " .. type(ast) .. ")")

	self.ast = ast

	return self:transpileAst(ast, false)
end

return Transpiler