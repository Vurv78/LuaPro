local lupa = require "src.lupa"

local TokenVariant, NodeVariant = lupa.Token.Variant, lupa.Node.Variant

local fmt, concat, gsub, ipairs = string.format, table.concat, string.gsub, ipairs

---@generic T, T2
---@param tbl T[]
---@param fn fun(t: T, integer): T2
local function map(tbl, fn) ---@return T2[]
	local out = {}
	for k, v in ipairs(tbl) do out[k] = fn(v, k) end
	return out
end

local format ---@type fun(node: Node): string

local function block(node --[[@param node Node]])
	if #node.data == 0 then return " " end
	return "\n\t" .. gsub(format(node), "\n", "\n\t") .. "\n"
end

function format(node --[[@param node Node]]) ---@return string
	local variant, data = node.variant, node.data
	if variant == NodeVariant.Chunk then ---@cast data Node[]
		return concat(map(data, format), "\n")
	elseif variant == NodeVariant.Comment then ---@cast data string
		return fmt("-- %s", data)
	elseif variant == NodeVariant.If then
		local buf = { fmt("if %s then%s", format(data[1][1]), block(data[1][2])) }
		for i = 2, #data do
			local chain = data[i]
			if chain[1] then
				buf[i] = fmt("elseif %s then%s", format(chain[1]), block(chain[2]))
			else
				buf[i] = "else" .. block(chain[2])
			end
		end
		return concat(buf) .. "end"
	elseif variant == NodeVariant.While then ---@cast data { [1]: Node, [2]: Node }
		return fmt("while %s do%send", format(data[1]), block(data[2]))
	elseif variant == NodeVariant.For then ---@cast data { [1]: "in"|"=" }
		if data[1] == "in" then ---@cast data { [1]: "in"|"=", [2]: Token<string>[], [3]: Node[] }
			local vars, vals = map(data[2], function(t) return t.data end), map(data[3], format)
			return fmt("for %s in %s do%send", concat(vars, ", "), concat(vals, ", "), block(data[4]))
		else ---@cast data { [1]: "in"|"=", [2]: Token<string>[], [3]: Node, [4]: Node, [5]: Node?, [6]: Node }
			local vars = map(data[2], function(t) return t.data end)
			return fmt("for %s = %s, %s, %s do%send", concat(vars, ", "), format(data[3]), format(data[4]), data[5] and format(data[5]) or "1", block(data[6]))
		end
	elseif variant == NodeVariant.Repeat then ---@cast data { [1]: Node, [2]: Node }
		return fmt("repeat%suntil %s", block(data[1]), format(data[2]))
	elseif variant == NodeVariant.Goto then ---@cast data Token<string>
		return "goto " .. data.data
	elseif variant == NodeVariant.Do then ---@cast data Node
		return fmt("do%send", block(data))
	elseif variant == NodeVariant.Return then ---@cast data Node[]?
		if data then
			return "return " .. concat(map(data, format), ", ")
		else
			return "return"
		end
	elseif variant == NodeVariant.Break then
		return "break"
	elseif variant == NodeVariant.Function then ---@cast data { [1]: boolean, [2]: { [1]: boolean, [2]: Token<string> }[], [3]: Token<string>?, [4]: Node }
		local path = {} ---@type string[]
		for i, part in ipairs(data[2]) do
			path[i] = (i ~= 1 and (part[1] and ":" or ".") or "") .. part[2].data
		end

		local parameters = map(data[3], function(t) return t.data or "..." end)
		return fmt("%sfunction %s(%s)%send", data[1] and "local " or "", concat(path), concat(parameters, ", "), block(data[4]))
	elseif variant == NodeVariant.Label then ---@cast data Token<string>
		return "::" .. data.data .. "::"
	elseif variant == NodeVariant.Assign then ---@cast data { [1]: { [1]: Node, [2]: Node[] }[], [2]: Node[] }
		local vars = map(data[1], function(var)
			local total = map(var[2], function(i) return "[" .. format(i) .. "]" end)
			return format(var[1]) .. concat(total)
		end)

		return fmt("%s = %s", concat(vars, ", "), concat(map(data[2], format), ", "))
	elseif variant == NodeVariant.LocalAssign then ---@cast data { [1]: Token<string>[], [2]: Node[]? }
		local idents = map(data[1], function(t) return t.data end)
		if data[2] then -- actually assigning values.
			return fmt("local %s = %s", concat(idents, ", "), concat(map(data[2], format), ", "))
		else
			return fmt("local %s", concat(idents, ", "))
		end
	elseif variant == NodeVariant.Grouped then ---@cast data Node
		return fmt("(%s)", format(data))
	elseif variant == NodeVariant.Addition then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s + %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Subtraction then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s - %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Multiply then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s * %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Divide then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s / %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Modulus then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s %% %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Pow then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s ^ %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Concat then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s .. %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Call then ---@cast data { [1]: Node, [2]: Node[] }
		local args = map(data[2], format)
		return fmt("%s(%s)", format(data[1]), concat(args, ", "))
	elseif variant == NodeVariant.MethodCall then ---@cast data { [1]: Node, [2]: Token<string>, [3]: Node[] }
		local args = map(data[3], format)
		return fmt("%s:%s(%s)", format(data[1]), data[2].data, concat(args, ", "))
	elseif variant == NodeVariant.Index then ---@cast data { [1]: Node, [2]: boolean, [3]: Node|Token }
		if data[2] then ---@cast data { [1]: Node, [2]: boolean, [3]: Token<string> }
			return fmt("%s.%s", format(data[1]), data[3].data)
		else ---@cast data { [1]: Node, [2]: boolean, [3]: Node }
			return fmt("%s[%s]", format(data[1]), format(data[3]))
		end
	elseif variant == NodeVariant.Or then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s or %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.And then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s and %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Equals then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s == %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.NotEquals then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s ~= %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.LessThan then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s < %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.LessThanEq then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s <= %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.GreaterThan then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s > %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.GreaterThanEq then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s >= %s", format(data[1]), format(data[2]))
	elseif variant == NodeVariant.Not then ---@cast data Node
		return fmt("not %s", format(data))
	elseif variant == NodeVariant.Negate then ---@cast data Node
		return fmt("-%s", format(data))
	elseif variant == NodeVariant.Length then ---@cast data Node
		return fmt("#%s", format(data))
	elseif variant == NodeVariant.Table then ---@cast data { [1]: Node?, [2]: Node }[]
		local contents = map(data, function(kv)
			if kv[1] then
				if kv[1].variant == NodeVariant.Identifier then
					return format(kv[1]) .. " = " .. format(kv[2]):gsub("\n", "\n\t")
				else
					return "[" .. format(kv[1]) .. "] = " .. format(kv[2]):gsub("\n", "\n\t")
				end
			else
				return format(kv[2])
			end
		end)

		if #contents < 4 then
			return "{" .. concat(contents, ", ") .. "}"
		else
			return "{\n\t" .. concat(contents, ",\n\t") .. "\n}"
		end
	elseif variant == NodeVariant.Literal then
		if data[1] == TokenVariant.String then ---@cast data { [1]: TokenVariant, [2]: Token<string> }
			return fmt("%q", data[2].data)
		elseif data[1] == TokenVariant.MString then ---@cast data { [1]: TokenVariant, [2]: Token<{ [1]: string, [2]: integer }> }
			local part = ("="):rep(data[2].data[2])
			return fmt("[%s[%s]%s]", part, data[2].data[1], part)
		elseif data[1] == TokenVariant.Vararg then
			return "..."
		else ---@cast data { [1]: TokenVariant, [2]: Token<string> }
			return tostring(data[2].data)
		end
	elseif variant == NodeVariant.Lambda then ---@cast data { [1]: Token<string>[], [2]: Node }
		local params = map(data[1], function(t) return t.data or "..." end)
		if #data[2].data == 0 then
			return fmt("function(%s) end", concat(params, ", "))
		else
			return fmt("function(%s)%send", concat(params, ", "), block(data[2]))
		end
	elseif variant == NodeVariant.Identifier then ---@cast data Token<string>
		return data.data
	end

	return "<todo" .. variant .. ">"
end

return {
	format = format,

	parse = lupa.parse,
	tokenize = lupa.tokenize,
}