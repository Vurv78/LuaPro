local Parser = require("parser/lua")

---@type Node
local Node = Parser.Node

---@type NodeKinds
local NODE_KINDS = Parser.Kinds

local fmt = string.format

-- Extends format mode
local Mode = assert( require("codegen/mode-lua/format") )

local Evaluators = {
	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Literal] = function(self, data)
		return data[3]
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.UnaryOps] = function(self, data)
		---@type string
		local op = data[1]
		---@type Node
		local expr = data[2]

		if expr.kind == NODE_KINDS.Literal then
			if op == "not" then
				local newval = not expr.data[3]
				return Node.new(NODE_KINDS.Literal, {"boolean", tostring(newval), newval})
			elseif expr.data[1] == "number" then
				-- negation (-), only works on numbers, errors on every other type.
				local newval = -expr.data[3]
				return  Node.new(NODE_KINDS.Literal, {"number", tostring(newval), newval})
			end
		end
	end,
}

local function evaluate(self, data, kind)
	local evaluator = Evaluators[kind]
	if evaluator then
		local ret = evaluator(self, data)
		return ret
	end
end

Mode[NODE_KINDS.If] = function(self, data)
	local cond_blocks, else_block = data[1], data[2]

	local n_conditions = #cond_blocks
	if n_conditions == 1 then
		-- Just the single `if`.
		if else_block then
			local cond, block = cond_blocks[1][1], cond_blocks[1][2]

			if evaluate(self, cond.data, cond.kind) then
				return fmt("do\n%s\nend", self:transpileAst(block, true))
			else
				return fmt("if %s then\n%s\nelse\n%s\nend", self:transpile(cond), self:transpileAst(block), self:transpileAst(else_block))
			end
		else
			local cond, block = cond_blocks[1][1], cond_blocks[1][2]

			if evaluate(self, cond.data, cond.kind) then
				return fmt("do\n%s\nend", self:transpileAst(block, true))
			else
				return fmt("if %s then\n%s\nend", self:transpile(cond), self:transpileAst(block))
			end
		end
	else
		if else_block then
			local rest = {}
			for k, tbl in ipairs(cond_blocks) do
				local condition, block = tbl[1], tbl[2]
				rest[k] = fmt("%sif %s then\n%s\n", k ~= 1 and "else" or "", self:transpile(condition), self:transpileAst(block))
			end

			return table.concat(rest, "") .. "else\n" .. self:transpileAst(else_block) .. "\nend"
		else
			local rest = {}
			for k, tbl in ipairs(cond_blocks) do
				local condition, block = tbl[1], tbl[2]
				rest[k] = fmt("%sif %s then\n%s\n", k ~= 1 and "else" or "", self:transpile(condition), self:transpileAst(block))
			end

			return table.concat(rest, "") .. "end"
		end
	end
end

return Mode