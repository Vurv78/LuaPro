--[[
	Lupa - Tiny lua parser in lua.
	By Vurv78 (https://github.com/Vurv78)
]]

--#region tokenizer

---@class Token<T>: { data: T, variant: TokenVariant }
---@field variant TokenVariant
---@field data unknown # Default generic T = unknown
local Token = {}
Token.__index = Token

---@param variant TokenVariant
---@generic T
---@param data T
---@return Token<T>
function Token.new(variant, data)
	return setmetatable({ variant = variant, data = data }, Token)
end

function Token:__tostring()
	return "Token { variant: " .. self.variant .. ", data: " .. tostring(self.data) .. " }"
end

---@enum TokenVariant
local TokenVariant = {
	Keyword = 1, Identifier = 2,

	String = 3, MString = 4,
	Comment = 5, MComment = 6,

	Integer = 7, Decimal = 8,
	Hexadecimal = 9, Binary = 10,

	Boolean = 11, Nil = 12,
	Operator = 13, Grammar = 14,
	Vararg = 15, Label = 16
}

Token.Variant = TokenVariant

local Keywords = {
	["while"] = true, ["if"] = true, ["elseif"] = true, ["else"] = true,
	["do"] = true, ["then"] = true, ["repeat"] = true, ["local"] = true,
	["function"] = true, ["return"] = true, ["goto"] = true,
	["end"] = true, ["for"] = true, ["in"] = true, ["until"] = true,
	["break"] = true
}

---@param src string
---@return Token[]
local function tokenize(src)
	local ptr, len = 1, #src

	local function skipWhitespace()
		local _, ed = src:find("^(%s+)", ptr)
		if ed then
			ptr = ed + 1
		end
	end

	---@param pattern string
	---@return string?
	local function consume(pattern)
		local _, ed, match = src:find(pattern, ptr)

		if ed then
			ptr = ed + 1
			return match or true
		end
	end

	local function next()
		skipWhitespace()

		local comment, data = consume("^%-%-"), consume("^%[(=*)%[")
		if data then
			local ending_part = "]" .. ("="):rep(#data) .. "]"
			local start, ed = src:find(ending_part, ptr, true)
			if start then
				local inner = src:sub(ptr, start - 1)
				ptr = ed + 1
				return Token.new(comment and TokenVariant.MComment or TokenVariant.MString, inner)
			else
				error("Missing " .. ending_part .. " to end multiline " .. (comment and "comment" or "string"))
			end
		elseif comment then
			return Token.new(TokenVariant.Comment, consume("^([^\n]+)\n"))
		end

		local data = consume("^(%d+%.%d+)")
		if data then
			return Token.new(TokenVariant.Decimal, tonumber(data))
		end

		local data = consume("^(0[xX]%x+)")
		if data then
			return Token.new(TokenVariant.Hexadecimal, tonumber(data, 16))
		end

		local data = consume("^0b([01]+)")
		if data then
			return Token.new(TokenVariant.Binary, tonumber(data, 2))
		end

		local data = consume("^(%d+)")
		if data then
			return Token.new(TokenVariant.Integer, tonumber(data))
		end

		local buffer = {} -- Identifier / Keywords support unicode
		repeat
			local unicode = consume("^([%z\x01-\x7F\xC2-\xF4][\x80-\xBF]*)") -- utf8.charpattern
			if not unicode then break end

			if string.byte(unicode) < 128 and unicode:match("^[^%w_]") then -- Ascii symbol
				ptr = ptr - 1
				break
			end

			buffer[#buffer + 1] = unicode
		until ptr >= len

		if #buffer > 0 then
			local data = table.concat(buffer)
			if Keywords[data] then
				return Token.new(TokenVariant.Keyword, data)
			elseif data == "true" or data == "false" then
				return Token.new(TokenVariant.Boolean, data == "true")
			elseif data == "nil" then
				return Token.new(TokenVariant.Nil, nil)
			elseif data == "and" or data == "or" or data == "not" then
				return Token.new(TokenVariant.Operator, data)
			else
				return Token.new(TokenVariant.Identifier, data)
			end
		end

		local data = consume('^\"([^\"]*)\"') -- Todo: Escapes
		if data then
			return Token.new(TokenVariant.String, data)
		end

		local data = consume("^'([^']*)'") -- Todo: Escapes
		if data then
			return Token.new(TokenVariant.String, data)
		end

		local data = consume("^::([%w_]+)::")
		if data then
			return Token.new(TokenVariant.Label, data)
		end

		local op = consume("^([!~=><]=)")
		if op then
			return Token.new(TokenVariant.Operator, op)
		end

		local op = consume("^([%+%-%*%/%%=%^%<%>%#%!])")
		if op then
			return Token.new(TokenVariant.Operator, op)
		end

		if consume("^%.%.%.") then
			return Token.new(TokenVariant.Vararg, nil)
		elseif consume("^%.%.") then
			return Token.new(TokenVariant.Operator, "..")
		elseif consume("^&&") then
			return Token.new(TokenVariant.Operator, "&&")
		elseif consume("^||") then
			return Token.new(TokenVariant.Operator, "||")
		end

		local grammar = consume("^([%(%)%{%}%[%]%,%.%;%:])")
		if grammar then
			return Token.new(TokenVariant.Grammar, grammar)
		end
	end

	local tokens = {}
	while ptr <= len do
		skipWhitespace()
		if ptr > len then return tokens end

		local t = next()
		if not t then
			error("Failed to parse token " .. (consume("^(%S+)") or "EOF"))
		end

		if t.variant ~= TokenVariant.Comment and t.variant ~= TokenVariant.MComment then
			tokens[#tokens + 1] = t
		end
	end

	return tokens
end

--#endregion

--#region parser
---@class Node
---@field variant NodeVariant
---@field data any
local Node = {}
Node.__index = Node

---@param variant NodeVariant
---@param data any
---@return Node
function Node.new(variant, data)
	return setmetatable({ variant = variant, data = data }, Node)
end

function Node:__tostring()
	return "Node { variant: " .. self.variant .. ", data: " .. tostring(self.data) .. " }"
end

---@enum NodeVariant
local NodeVariant = {
	Chunk = -1, Comment = 0,

	If = 1, While = 2, For = 3,
	Repeat = 4, Goto = 5, Do = 6, Return = 7,
	Break = 8, Function = 9, Label = 10,

	Assign = 11, LocalAssign = 12,

	Grouped = 13, Addition = 14, Subtraction = 15, Multiply = 16,
	Divide = 17, Modulus = 18, Pow = 19, Concat = 20,

	Call = 21, MethodCall = 22, Index = 23,

	Or = 24, And = 25,

	Equals = 26, NotEquals = 27, LessThan = 28,
	LessThanEq = 29, GreaterThan = 30, GreaterThanEq = 31,

	Not = 32, Negate = 33, Length = 34,

	Literal = 35, Lambda = 36, Identifier = 37
}

Node.Variant = NodeVariant

---@alias Scope { labels: table<string, boolean>, pending_gotos: table<string, Token> }

---@param tokens Token[]
---@return Node
local function parse(tokens)
	local index, len = 1, #tokens

	local scopes, scopeid = {}, 1 ---@type Scope[], integer # Scoping for labels, variadic functions

	local function pushScope()
		scopes[scopeid + 1], scopeid = {
			labels = setmetatable({}, { __index = scopes[scopeid] and scopes[scopeid].labels }),
			pending_gotos = {}
		}, scopeid + 1
	end

	local function popScope()
		for name, _ in pairs(scopes[scopeid].pending_gotos) do -- todo: when traces are saved, use token.
			assert(scopes[scopeid].labels[name], "No visible label '" .. name .. "'")
		end
		scopes[scopeid], scopeid = nil, scopeid - 1
	end

	---@generic T
	---@param variant TokenVariant
	---@param data T
	---@return Token<T>?
	---@overload fun(variant: TokenVariant): Token?
	local function consume(variant, data)
		local p = tokens[index]
		if p and p.variant == variant then
			if data ~= nil and p.data ~= data then
				return
			end
			index = index + 1
			return p
		end
	end

	---@param variant TokenVariant
	---@return boolean
	local function peek(variant)
		return tokens[index] and tokens[index].variant == variant
	end

	local arguments, parameters, stmt, expr, block, namelist, varlist, explist
	local function prim()
		local n = consume(TokenVariant.Nil)
		if n then
			return Node.new(NodeVariant.Literal, { "nil", n })
		end

		local bool = consume(TokenVariant.Boolean)
		if bool then
			return Node.new(NodeVariant.Literal, { "boolean", bool })
		end

		local n = consume(TokenVariant.Integer) or consume(TokenVariant.Hexadecimal) or consume(TokenVariant.Binary)
		if n then
			return Node.new(NodeVariant.Literal, { "int", n })
		end

		local n = consume(TokenVariant.Decimal)
		if n then
			return Node.new(NodeVariant.Literal, { "float", n })
		end

		local str = consume(TokenVariant.String) or consume(TokenVariant.MString)
		if str then
			return Node.new(NodeVariant.Literal, { "string", str })
		end

		local vararg = consume(TokenVariant.Vararg)
		if vararg then
			return Node.new(NodeVariant.Literal, { "...", vararg })
		end

		local fn = consume(TokenVariant.Keyword, "function")
		if fn then
			local params = assert(parameters(), "Expected parameters to follow function keyword")
			return Node.new(NodeVariant.Lambda, {params, assert(block("end"), "Expected block to follow lambda")})
		end
	end

	local function prefixexp() ---@return Node?
		local ident = consume(TokenVariant.Identifier)
		if ident then
			return Node.new(NodeVariant.Identifier, ident)
		end

		if consume(TokenVariant.Grammar, "(") then
			local exp = assert(expr(), "Expected expression for grouped expression")
			assert(consume(TokenVariant.Grammar, ")"), "Expected ) to close grouped expression")
			return Node.new(NodeVariant.Grouped, exp)
		end
	end

	local function field() ---@return { [1]: Node?, [2]: Node }
		if consume(TokenVariant.Grammar, "[") then
			local key = assert(expr(), "Expected expression for field key")
			assert(consume(TokenVariant.Grammar, "]"), "Expected ] to close field key")
			assert(consume(TokenVariant.Operator, "="), "Expected = to follow field key expression")
			return { key, assert(expr(), "Expected expression for field value") }
		else
			local before, id = index, consume(TokenVariant.Identifier)
			if id then
				if consume(TokenVariant.Operator, "=") then
					local key = Node.new(NodeVariant.Identifier, id)
					return { key, assert(expr(), "Expected expression for field value") }
				else -- expression
					index = before
					return { nil, expr() }
				end
			else
				return { nil, assert(expr(), "Expected expression for field") }
			end
		end
	end

	local function fieldsep()
		return consume(TokenVariant.Grammar, ",") or consume(TokenVariant.Grammar, ";")
	end

	local function tableconstructor() ---@return Node?
		if consume(TokenVariant.Grammar, "{") then
			if consume(TokenVariant.Grammar, "}") then return Node.new(NodeVariant.Literal, { "table", {} }) end
			local fields = {}
			repeat
				fields[#fields + 1] = field()
				fieldsep()
			until consume(TokenVariant.Grammar, "}")
			return Node.new(NodeVariant.Literal, { "table", fields })
		end
	end

	function expr() -- nil | boolean | number | string | ... | function | prefixexp | tableconstructor | exp binop exp | unop exp
		local p = prim() or prefixexp() or tableconstructor()

		if not p then
			if consume(TokenVariant.Operator, "not") or consume(TokenVariant.Operator, "!") then
				return Node.new(NodeVariant.Not, expr())
			elseif consume(TokenVariant.Operator, "-") then
				return Node.new(NodeVariant.Negate, expr())
			elseif consume(TokenVariant.Operator, "#") then
				return Node.new(NodeVariant.Length, expr())
			else
				return
			end
		end

		while true do -- Should be implemented in prefixexp() according to EBNF grammar, but that uses left recursion.
			if consume(TokenVariant.Grammar, "(") then
				p = Node.new(NodeVariant.Call, { p, assert(arguments("call expression", ")"), "Expected args") })
			elseif consume(TokenVariant.Grammar, "[") then
				assert(p.variant == NodeVariant.Identifier or p.variant == NodeVariant.Grouped or p.variant == NodeVariant.Call or p.variant == NodeVariant.MethodCall or p.variant == NodeVariant.Index, "Cannot index expr (" .. tostring(p) .. "), wrap it in parenthesis")
				p = Node.new(NodeVariant.Index, { p, false, assert(expr(), "Expected expression for index") })
				assert(consume(TokenVariant.Grammar, "]"), "Expected ] to close index")
			elseif consume(TokenVariant.Grammar, ".") then
				assert(p.variant == NodeVariant.Identifier or p.variant == NodeVariant.Grouped or p.variant == NodeVariant.Call or p.variant == NodeVariant.MethodCall or p.variant == NodeVariant.Index, "Cannot index expr (" .. tostring(p) .. "), wrap it in parenthesis")
				p = Node.new(NodeVariant.Index, { p, true, assert(consume(TokenVariant.Identifier), "Expected identifier for dot index") })
			elseif consume(TokenVariant.Grammar, ":") then
				local method = assert(consume(TokenVariant.Identifier), "Expected method name after :")
				assert(consume(TokenVariant.Grammar, "("), "Expected ( to start methodcall arguments")
				p = Node.new(NodeVariant.MethodCall, { p, method, arguments("method call", ")") })
			elseif p and p.variant == NodeVariant.Identifier then
				local str = consume(TokenVariant.String) or consume(TokenVariant.MString)
				if str then
					p = Node.new(NodeVariant.Call, { p, { Node.new(NodeVariant.Literal, { "string", str }) } })
				else
					local table = tableconstructor()
					if table then
						p = Node.new(NodeVariant.Call, { p, { table } })
					else
						break
					end
				end
			else
				break
			end
		end

		if consume(TokenVariant.Operator, "+") then
			return Node.new(NodeVariant.Addition, { p, expr() })
		elseif consume(TokenVariant.Operator, "-") then
			return Node.new(NodeVariant.Subtraction, { p, expr() })
		elseif consume(TokenVariant.Operator, "*") then
			return Node.new(NodeVariant.Multiply, { p, expr() })
		elseif consume(TokenVariant.Operator, "/") then
			return Node.new(NodeVariant.Divide, { p, expr() })
		elseif consume(TokenVariant.Operator, "%") then
			return Node.new(NodeVariant.Modulus, { p, expr() })
		elseif consume(TokenVariant.Operator, "^") then
			return Node.new(NodeVariant.Pow, { p, expr() })
		elseif consume(TokenVariant.Operator, "..") then
			return Node.new(NodeVariant.Concat, { p, expr() })
		elseif consume(TokenVariant.Operator, "or") or consume(TokenVariant.Operator, "||") then
			return Node.new(NodeVariant.Or, { p, expr() })
		elseif consume(TokenVariant.Operator, "and") or consume(TokenVariant.Operator, "&&") then
			return Node.new(NodeVariant.And, { p, expr() })
		elseif consume(TokenVariant.Operator, "<") then
			return Node.new(NodeVariant.LessThan, { p, expr() })
		elseif consume(TokenVariant.Operator, "<=") then
			return Node.new(NodeVariant.LessThanEq, { p, expr() })
		elseif consume(TokenVariant.Operator, ">") then
			return Node.new(NodeVariant.GreaterThan, { p, expr() })
		elseif consume(TokenVariant.Operator, ">=") then
			return Node.new(NodeVariant.GreaterThanEq, { p, expr() })
		elseif consume(TokenVariant.Operator, "==") then
			return Node.new(NodeVariant.Equals, { p, expr() })
		elseif consume(TokenVariant.Operator, "~=") or consume(TokenVariant.Operator, "!=") then
			return Node.new(NodeVariant.NotEquals, { p, expr() })
		end

		return p
	end


	---@param ending "end"|"until"|"elseif"|"else"
	---@return Node[]?
	function block(ending)
		local nodes, old = {}, index
		pushScope()
		while index <= len do
			if consume(TokenVariant.Keyword, ending) then
				popScope()
				return Node.new(NodeVariant.Chunk, nodes)
			end

			local node = stmt()
			if not node then break end

			while consume(TokenVariant.Grammar, ";") do end
			nodes[#nodes + 1] = node
		end

		popScope()
		index = old
	end

	---@param msg string?
	---@param ending string?
	---@return Node[]
	function arguments(msg, ending)
		local exprs = {}
		if ending and consume(TokenVariant.Grammar, ending) then return exprs end
		repeat
			exprs[#exprs + 1] = assert(expr(), "Expected expression for " .. (msg or "arguments"))
			if ending and consume(TokenVariant.Grammar, ending) then return exprs end
		until not consume(TokenVariant.Grammar, ",")
		return exprs
	end

	---@param msg string?
	---@return Token[]
	function namelist(msg)
		local idents = {}
		repeat
			idents[#idents + 1] = assert(consume(TokenVariant.Identifier), "Expected identifier for " .. (msg or "parameter"))
		until not consume(TokenVariant.Grammar, ",")
		return idents
	end

	---@return Node[]?
	function explist()
		local exprs, before = {}, index
		repeat
			local e = expr()
			if not e then
				index = before
				return
			end
			exprs[#exprs + 1] = e
		until not consume(TokenVariant.Grammar, ",")
		return exprs
	end

	---@return { [1]: Node, [2]: Node[] }?
	local function var() -- Name | prefixexp "[" exp "]" | prefixexp "." Name
		local ident, index = prefixexp()
		if not ident then return end

		local chain, i = {}, 1 ---@type Node[]
		while true do
			if consume(TokenVariant.Grammar, "[") then
				local key = assert(expr(), "Expected expression for index key")
				assert(consume(TokenVariant.Grammar, "]"), "Expected ] to end index")
				chain[i], i = key, i + 1
			elseif consume(TokenVariant.Grammar, ".") then
				chain[i], i = Node.new(NodeVariant.Literal, {"string", assert(consume(TokenVariant.Identifier), "Expected identifier for dot index")}), i + 1
			else
				break
			end
		end

		return { ident, chain }
	end

	---@param msg string
	---@return { [1]: Node, [2]: Node? }[]
	function varlist(msg)
		local vars = {}
		repeat
			vars[#vars + 1] = assert(var(), "Expected identifier for " .. msg)
		until not consume(TokenVariant.Grammar, ",")
		return vars
	end

	---@return Token[]?
	function parameters()
		if not consume(TokenVariant.Grammar, "(") then return end
		if consume(TokenVariant.Grammar, ")") then return {} end

		local params = {}
		repeat
			local v = consume(TokenVariant.Vararg)
			if v then
				assert(consume(TokenVariant.Grammar, ")"), "Variadic parameter must be last parameter")
				params[#params + 1] = v
				return params
			end
			params[#params + 1] = assert(consume(TokenVariant.Identifier), "Expected identifier or ... for function parameter")
		until not consume(TokenVariant.Grammar, ",")
		assert(consume(TokenVariant.Grammar, ")"), "Expected ) to close function parameters")

		return params
	end

	function stmt()
		if consume(TokenVariant.Keyword, "while") then
			local cond = assert(expr(), "Expected expression to follow while keyword")
			assert(consume(TokenVariant.Keyword, "do"), "Expected 'do' keyword after while expression")

			return Node.new(NodeVariant.While, { cond, assert(block("end"), "Expected 'end' to mark end of while loop") })
		elseif consume(TokenVariant.Keyword, "for") then
			local vars = namelist("for loop")
			if consume(TokenVariant.Keyword, "in") then -- for x, y, z in pairs()
				local vals = assert(explist(), "Expected expression in for loop statement")
				assert(consume(TokenVariant.Keyword, "do"), "Expected do keyword following for loop values")
				return Node.new(NodeVariant.For, { "in", vars, vals, assert(block("end"), "Expected 'end' to mark end of for loop") })
			elseif consume(TokenVariant.Operator, "=") then
				local start = assert(expr(), "Expected expression as for loop start")
				assert(consume(TokenVariant.Grammar, ","), "Expected comma to follow for loop start expression")
				local ed = assert(expr(), "Expected expression as for loop ending")

				local jmp
				if consume(TokenVariant.Grammar, ",") then
					jmp = assert(expr(), "Expected expression after , as for loop jump expression")
				end

				assert(consume(TokenVariant.Keyword, "do"), "Expected do keyword following for loop values")
				return Node.new(NodeVariant.For, { "=", vars, start, ed, jmp, block("end") })
			else
				error("Expected '=' operator or 'in' keyword to follow for loop identifier(s)")
			end
		elseif consume(TokenVariant.Keyword, "if") then -- todo: elseif / else
			local chain = {} ---@type { [1]: Node?, [2]: Node }
			while true do
				local cond = assert(expr(), "Expected condition for if statement")
				assert(consume(TokenVariant.Keyword, "then"), "Expected 'then' keyword after condition")

				local b = block("end")
				if b then
					chain[#chain + 1] = { cond, b }
					return Node.new(NodeVariant.If, chain)
				else
					local b = block("elseif")
					if b then
						chain[#chain + 1] = { cond, b }
					else
						chain[#chain + 1] = { cond, assert(block("else"), "Expected block following if condition") }
						chain[#chain + 1] = { nil, assert(block("end"), "Expected block following else keyword") }
						return Node.new(NodeVariant.If, chain)
					end
				end
			end
		elseif consume(TokenVariant.Keyword, "do") then
			return Node.new(NodeVariant.Do, block("end"))
		elseif consume(TokenVariant.Keyword, "repeat") then
			return Node.new(NodeVariant.Repeat, { assert(block("until"), "Expected block after repeat"), expr() })
		elseif consume(TokenVariant.Keyword, "break") then
			return Node.new(NodeVariant.Break, nil)
		elseif consume(TokenVariant.Identifier, "continue") then
			return Node.new(NodeVariant.Goto, Token.new(TokenVariant.Label, "__continue__"))
		elseif consume(TokenVariant.Keyword, "function") then
			local path = {} ---@type { [1]: boolean, [2]: Token<string> }[]
			repeat
				path[#path + 1] = { false, assert(consume(TokenVariant.Identifier), "Expected identifier for function name") }
			until not consume(TokenVariant.Grammar, ".")

			if consume(TokenVariant.Grammar, ":") then
				path[#path + 1] = { true, assert(consume(TokenVariant.Identifier), "Expected identifier after : for method name") }
			end

			return Node.new(NodeVariant.Function, {
				false,
				path,
				assert(parameters(), "Expected parameters following function name"),
				assert(block("end"), "Expected block following function parameters")
			})
		elseif consume(TokenVariant.Keyword, "local") then
			if consume(TokenVariant.Keyword, "function") then
				return Node.new(NodeVariant.Function, {
					true,
					{ { false, assert(consume(TokenVariant.Identifier), "Expected identifier for function name") } },
					assert(parameters(), "Expected parameters following function name"),
					assert(block("end"), "Expected block following function parameters")
				})
			else
				local idents = namelist("local declaration")
				if consume(TokenVariant.Operator, "=") then
					return Node.new(NodeVariant.LocalAssign, { idents, assert(explist(), "Expected expression for local declaration") })
				end
				return Node.new(NodeVariant.LocalAssign, { idents })
			end
		elseif consume(TokenVariant.Keyword, "goto") then
			local label = assert(consume(TokenVariant.Identifier), "Expected label after goto")
			if not scopes[scopeid].labels[label.data] then -- Wait for end of scope to check if this label is still not defined.
				scopes[scopeid].pending_gotos[label.data] = label
			end
			return Node.new(NodeVariant.Goto, label)
		elseif consume(TokenVariant.Keyword, "return") then
			return Node.new(NodeVariant.Return, explist())
		else
			local label = consume(TokenVariant.Label)
			if label then
				scopes[scopeid].labels[label.data] = true
				return Node.new(NodeVariant.Label, label)
			end
		end

		local save = index
		if peek(TokenVariant.Identifier) then
			local paths = varlist("assignment")
			if consume(TokenVariant.Operator, "=") then
				return Node.new(NodeVariant.Assign, {paths, assert(explist(), "Expected expression(s) for assignment")})
			end
			index = save
		end


		local save, call = index, expr()
		if call and (call.variant == NodeVariant.Call or call.variant == NodeVariant.MethodCall) then
			return call
		else
			index = save
		end
	end

	local nodes = {}
	pushScope()
	while index <= len do
		local node = stmt()
		assert(node, "Failed to parse: '" .. tostring(tokens[index]) .. "'")

		while consume(TokenVariant.Grammar, ";") do end
		nodes[#nodes + 1] = node
	end
	popScope()

	return Node.new(NodeVariant.Chunk, nodes)
end

--#endregion

--#region display

local fmt, concat, ipairs = string.format, table.concat, ipairs

---@generic T
---@generic T2
---@param tbl T[]
---@param fn fun(t: T): T2
---@return T2[]
local function map(tbl, fn)
	local out = {}
	for i, v in ipairs(tbl) do
		out[i] = fn(v)
	end
	return out
end

---@param node Node
---@return string
local function block(node)
	if #node.data == 0 then return " " end
	return "\n\t" .. node:display():gsub("\n", "\n\t") .. "\n"
end

function Node:display()
	local variant, data = self.variant, self.data
	if variant == NodeVariant.Chunk then ---@cast data Node[]
		return concat(map(data, Node.display), "\n")
	elseif variant == NodeVariant.Comment then ---@cast data string
		return fmt("-- %s", data)
	elseif variant == NodeVariant.If then
		local first = table.remove(data, 1)
		local first = fmt("if %s then%s", first[1]:display(), block(first[2]))

		local buf = map(data, function (chain)
			if chain[1] then
				return fmt("elseif %s then%s", chain[1]:display(), block(chain[2]))
			else
				return "else" .. block(chain[2])
			end
		end)

		return first .. concat(buf) .. "end"
	elseif variant == NodeVariant.While then ---@cast data { [1]: Node, [2]: Node }
		return fmt("while %s do%send", data[1]:display(), block(data[2]))
	elseif variant == NodeVariant.For then ---@cast data { [1]: "in"|"=" }
		if data[1] == "in" then ---@cast data { [1]: "in"|"=", [2]: Token<string>[], [3]: Node[] }
			local vars, vals = map(data[2], function(t) return t.data end), map(data[3], Node.display)
			return fmt("for %s in %s do%send", concat(vars, ", "), concat(vals, ", "), block(data[4]))
		else ---@cast data { [1]: "in"|"=", [2]: Token<string>[], [3]: Node, [4]: Node, [5]: Node?, [6]: Node }
			local vars = map(data[2], function(t) return t.data end)
			return fmt("for %s = %s, %s, %s do%send", concat(vars, ", "), data[3]:display(), data[4]:display(), data[5] and data[5]:display() or "1", block(data[6]))
		end
	elseif variant == NodeVariant.Repeat then ---@cast data { [1]: Node, [2]: Node }
		return fmt("repeat%suntil %s", block(data[1]), data[2]:display())
	elseif variant == NodeVariant.Goto then ---@cast data Token<string>
		return "goto " .. data.data
	elseif variant == NodeVariant.Do then ---@cast data Node
		return fmt("do%send", block(data))
	elseif variant == NodeVariant.Return then ---@cast data Node[]?
		if data then
			return "return " .. concat(map(data, Node.display), ", ")
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
			local total = map(var[2], function(i) return "[" .. i:display() .. "]" end)
			return var[1]:display() .. concat(total)
		end)

		return fmt("%s = %s", concat(vars, ", "), concat(map(data[2], Node.display), ", "))
	elseif variant == NodeVariant.LocalAssign then ---@cast data { [1]: Token<string>[], [2]: Node[]? }
		local idents = map(data[1], function(t) return t.data end)
		if data[2] then -- actually assigning values.
			return fmt("local %s = %s", concat(idents, ", "), concat(map(data[2], Node.display), ", "))
		else
			return fmt("local %s", concat(idents, ", "))
		end
	elseif variant == NodeVariant.Grouped then ---@cast data Node
		return fmt("(%s)", data:display())
	elseif variant == NodeVariant.Addition then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s + %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Subtraction then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s - %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Multiply then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s * %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Divide then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s / %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Modulus then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s %% %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Pow then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s ^ %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Concat then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s .. %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Call then ---@cast data { [1]: Node, [2]: Node[] }
		local args = map(data[2], Node.display)
		return fmt("%s(%s)", data[1]:display(), concat(args, ", "))
	elseif variant == NodeVariant.MethodCall then ---@cast data { [1]: Node, [2]: Token<string>, [3]: Node[] }
		local args = map(data[3], Node.display)
		return fmt("%s:%s(%s)", data[1]:display(), data[2].data, concat(args, ", "))
	elseif variant == NodeVariant.Index then ---@cast data { [1]: Node, [2]: boolean, [3]: Node|Token }
		if data[2] then ---@cast data { [1]: Node, [2]: boolean, [3]: Token<string> }
			return fmt("%s.%s", data[1]:display(), data[3].data)
		else ---@cast data { [1]: Node, [2]: boolean, [3]: Node }
			return fmt("%s[%s]", data[1]:display(), data[3]:display())
		end
	elseif variant == NodeVariant.Or then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s or %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.And then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s and %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Equals then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s == %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.NotEquals then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s ~= %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.LessThan then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s < %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.LessThanEq then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s <= %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.GreaterThan then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s > %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.GreaterThanEq then ---@cast data { [1]: Node, [2]: Node }
		return fmt("%s >= %s", data[1]:display(), data[2]:display())
	elseif variant == NodeVariant.Not then ---@cast data Node
		return fmt("not %s", data:display())
	elseif variant == NodeVariant.Negate then ---@cast data Node
		return fmt("-%s", data:display())
	elseif variant == NodeVariant.Length then ---@cast data Node
		return fmt("#%s", data:display())
	elseif variant == NodeVariant.Literal then
		if data[1] == "table" then ---@cast data { [1]: string, [2]: { [1]: Node?, [2]: Node }[] }
			local contents = map(data[2], function(kv)
				if kv[1] then
					if kv[1].variant == NodeVariant.Identifier then
						return kv[1]:display() .. " = " .. kv[2]:display():gsub("\n", "\n\t")
					else
						return "[" .. kv[1]:display() .. "] = " .. kv[2]:display():gsub("\n", "\n\t")
					end
				else
					return kv[2]:display()
				end
			end)

			if #contents < 4 then
				return "{" .. concat(contents, ", ") .. "}"
			else
				return "{\n\t" .. concat(contents, ",\n\t") .. "\n}"
			end
		elseif data[1] == "string" then ---@cast data { [1]: string, [2]: Token<string> }
			return fmt("%q", data[2].data)
		elseif data[1] == "..." then
			return "..."
		else ---@cast data { [1]: string, [2]: Token<string> }
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
--#endregion

return {
	tokenize = tokenize,
	parse = parse,

	Token = Token,
	Node = Node
}