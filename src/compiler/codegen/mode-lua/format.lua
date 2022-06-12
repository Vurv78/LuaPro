---@type NodeKinds
local NODE_KINDS = require("compiler/parser/lua").Kinds

local fmt = string.format

local Transpilers = {
	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Comment] = function(self, data)
		local multiline, inner, depth = data[1], data[2], data[3]

		if multiline then
			local equals = string.rep("=", depth)
			return fmt("--[%s[%s]%s]", equals, inner, equals)
		else
			return "--" .. inner
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.While] = function(self, data)
		local cond = self:transpile( data[1] )
		local body = self:transpileAst( data[2] )
		return fmt("while %s do\n%s\nend", cond, body)
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.If] = function(self, data)
		local cond, block = data[1], data[2]

		local next = self:peek()
		if next and (next.kind == NODE_KINDS.Else or next.kind == NODE_KINDS.Elseif) then
			return fmt("if %s then %s", self:transpile(cond), self:transpileAst(block))
		end

		return fmt("if %s then\n%s\nend", self:transpile(cond), self:transpileAst(block))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Elseif] = function(self, data)
		local cond, block = data[1], data[2]

		local next = self:peek()
		if next and (next.kind == NODE_KINDS.Else or next.kind == NODE_KINDS.Elseif) then
			return fmt("elseif %s then %s", self:transpile(cond), self:transpileAst(block))
		end

		return fmt("elseif %s then\n%s\nend", self:transpile(cond), self:transpileAst(block))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Else] = function(self, data)
		local block = data[1]
		return fmt("else\n%s\nend", self:transpileAst(block))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.For] = function(self, data)
		local var, start, ed, step = data[1], data[2], data[3], data[4]
		local body = self:transpileAst( data[5], true )

		if step then
			return fmt("for %s = %s, %s, %s do\n%s\nend", var, self:transpile(start), self:transpile(ed), self:transpile(step), body)
		else
			return fmt("for %s = %s, %s do\n%s\nend", var, self:transpile(start), self:transpile(ed), body)
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Function] = function(self, data)
		local is_local, tbl, idkind, name, args, block = data[1], data[2], data[3], data[4], data[5], data[6]

		if tbl then
			-- function foo.bar(), function foo:bar()
			return fmt("%sfunction %s%s%s(%s)\n%s\nend", is_local and "local " or "", tbl, idkind, name, table.concat(args, ", "), self:transpileAst(block))
		else
			-- function bar()
			return fmt("%sfunction %s(%s)\n%s\nend", is_local and "local " or "", name, table.concat(args, ", "), self:transpileAst(block))
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Repeat] = function(self, data)
		return fmt("repeat\n%s\nuntil %s", self:transpileAst(data[2]), self:transpile(data[1]))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Block] = function(self, data)
		local block = data[1]
		return fmt("do\n%s\nend", self:transpileAst(block))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Escape] = function(self, data)
		local kind = data[1]
		if kind == "break" then
			return "break"
		elseif kind == "goto" then
			return fmt("goto %s", data[2])
		else
			-- "return"
			if data[2] then
				-- Simply return
				return "return"
			else
				-- Return with value
				return fmt("return %s", self:transpile(data[2]))
			end
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.LVarDecl] = function(self, data)
		local names, vals = data[1], data[2]

		if vals then
			local valstrs = {}
			for i = 1, #vals do
				valstrs[i] = self:transpile(vals[i])
			end

			return fmt("local %s = %s", table.concat(names, ", "), table.concat(valstrs, ", "))
		else
			return fmt("local %s", table.concat(names, ", "))
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.VarAssign] = function(self, data)
		local names, vals = data[1], data[2]

		local valstrs = {}
		for i = 1, #vals do
			valstrs[i] = self:transpile(vals[i])
		end

		return fmt("%s = %s", table.concat(names, ", "), table.concat(valstrs, ", "))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Index] = function(self, data)
		local kind, tbl, key = data[1], data[2], data[3]
		if kind == "[]" then
			return fmt("%s[%s]", self:transpile(tbl), self:transpile(key))
		else
			-- Ident
			return fmt("%s.%s", self:transpile(tbl), key)
		end
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Call] = function(self, data)
		---@type Node
		local expr = data[1]

		---@type table<number, Node>
		local args = data[2]

		local argstrs = {}
		for i, arg in ipairs(args) do
			argstrs[i] = self:transpile(arg)
		end

		return fmt("%s(%s)", self:transpile(expr), table.concat(argstrs, ", "))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Table] = function(self, data)
		local out = {}
		for k, field in ipairs(data[1]) do
			local key, value = field[1], self:transpile(field[2])

			if type(key) == "number" then
				out[ k ] = value
			elseif type(key) == "string" then
				-- Ident
				out[ k ] = key .. " = " .. value
			else
				-- Expr
				out[ k ] = fmt("[%s] = %s", self:transpile(key), value)
			end
		end

		return "{" .. table.concat(out, ", ") .. "}"
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Lambda] = function(self, data)
		local args, block = data[1], data[2]
		return fmt("function(%s)\n%s\nend", table.concat(args, ", "), self:transpileAst(block))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.BinaryOps] = function(self, data)
		local op, lhs, rhs = data[1], data[2], data[3]
		return fmt("%s %s %s", self:transpile(lhs), op, self:transpile(rhs))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.UnaryOps] = function(self, data)
		local op, expr = data[1], data[2]
		return fmt("%s%s", op, self:transpile(expr))
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
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Identifier] = function(self, data)
		return data[1]
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.GroupedExpr] = function(self, data)
		return fmt("(%s)", self:transpile(data[1]))
	end,

	---@param self Transpiler
	---@param data table
	[NODE_KINDS.Label] = function(self, data)
		return "::" .. data[1] .. "::"
	end
}

return Transpilers