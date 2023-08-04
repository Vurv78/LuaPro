--[[
	Lupa - Tiny lua parser in lua.
	By Vurv78 (https://github.com/Vurv78)
]]

--#region tokenizer

---@alias Version "Lua 5.1"|"Lua 5.2"|"Lua 5.3"|"Lua 5.4"|"LuaJIT 2.1"|"Garry's Mod"
local UTF8_PATTERN = _VERSION == "Lua 5.1" and "^([%z\x01-\x7F\xC2-\xF4][\x80-\xBF]*)" or "^([\0\x01-\x7F\xC2-\xF4][\x80-\xBF]*)" -- utf8.charpattern

---@generic T
---@param v T?
---@return T
local function assert(v, msg)
	if not v then error(msg) end
	return v
end

---@class Token<T>: { data: T, variant: TokenVariant }
---@field variant TokenVariant
---@field data unknown # Default generic T = unknown
local Token = {}
Token.__index = Token

---@generic T
---@param data T
---@return Token<T>
function Token.new(variant --[[@param variant TokenVariant]], data)
	return setmetatable({ variant = variant, data = data }, Token)
end

function Token:__tostring()
	return "Token { variant: " .. self.variant .. ", data: " .. tostring(self.data) .. " }"
end

---@enum TokenVariant
local TokenVariant = {
	Identifier = 1,

	String = 2, MString = 3,
	Comment = 4, MComment = 5,

	Integer = 6, Decimal = 7,
	Hexadecimal = 8, Binary = 9, --[[LuaJIT 2.1]]

	Boolean = 10, Nil = 11,

	Keyword = 12, Operator = 13, Grammar = 14,
	Vararg = 15, Label = 16,
	Attribute = 17 --[[Lua 5.4]]
}

Token.Variant = TokenVariant

local Keywords = {
	["while"] = true, ["if"] = true, ["elseif"] = true, ["else"] = true,
	["do"] = true, ["then"] = true, ["repeat"] = true, ["local"] = true,
	["function"] = true, ["return"] = true, ["goto"] = true,
	["end"] = true, ["for"] = true, ["in"] = true, ["until"] = true,
	["break"] = true
}

local function tokenize(src --[[@param src string]], version --[[@param version Version?]]) ---@return Token[]
	version = version or _VERSION
	local unicode, has_attributes, c_ops, binary_literals = (version == "Garry's Mod" or version >= "LuaJIT 2.1"), version >= "Lua 5.4", version == "Garry's Mod", (version == "Garry's Mod" or version == "LuaJIT 2.1")

	local ptr, len = 1, #src

	local function skipWhitespace()
		local _, ed = src:find("^(%s+)", ptr)
		if ed then
			ptr = ed + 1
		end
	end

	---@param pattern string
	local function consume(pattern) ---@return string?
		local _, ed, match = src:find(pattern, ptr)

		if ed then
			ptr = ed + 1
			return match or true
		end
	end

	local ops_pattern = c_ops and "^([%+%-%*%/%%=%^%<%>%#!])" or "^([%+%-%*%/%%=%^%<%>%#])"
	local logical_ops_pattern = c_ops and "^([!~=><]=)" or "^([~=><]=)"

	local function next()
		skipWhitespace()

		local comment, data = consume("^%-%-"), consume("^%[(=*)%[")
		if data then
			local ending_part = "]" .. ("="):rep(#data) .. "]"
			local start, ed = src:find(ending_part, ptr, true)
			if start then
				local inner = src:sub(ptr, start - 1)
				ptr = ed + 1
				return Token.new(comment and TokenVariant.MComment or TokenVariant.MString, { inner, #data })
			else
				error("Missing " .. ending_part .. " to end multiline " .. (comment and "comment" or "string"))
			end
		elseif comment then
			return Token.new(TokenVariant.Comment, consume("^([^\n]+)\n"))
		end

		local data = consume("^(%d*%.%d+)")
		if data then
			return Token.new(TokenVariant.Decimal, tonumber(data))
		end

		local data = consume("^(0[xX]%x+)")
		if data then
			return Token.new(TokenVariant.Hexadecimal, tonumber(data, 16))
		end

		local data = binary_literals and consume("^0b([01]+)")
		if data then
			return Token.new(TokenVariant.Binary, tonumber(data, 2))
		end

		local data = consume("^(%d+e%d+)")
		if data then
			return Token.new(TokenVariant.Integer, tonumber(data))
		end

		local data = consume("^(%d+)")
		if data then
			return Token.new(TokenVariant.Integer, tonumber(data))
		end

		local buffer = {}
		if unicode then -- Identifier / Keywords support unicode in LuaJIT
			repeat
				local unicode = consume(UTF8_PATTERN)
				if not unicode then break end

				if string.byte(unicode) < 128 and unicode:match("^[^%w_]") then -- Ascii symbol
					ptr = ptr - 1
					break
				end

				buffer[#buffer + 1] = unicode
			until ptr > len
		else
			buffer[1] = consume("^([%a_][%w_]*)")
		end

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

		if has_attributes then
			local data = consume("^<(%w+)>")
			if data then
				return Token.new(TokenVariant.Attribute, data)
			end
		end

		local op = consume(logical_ops_pattern)
		if op then
			return Token.new(TokenVariant.Operator, op)
		end

		local op = consume(ops_pattern)
		if op then
			return Token.new(TokenVariant.Operator, op)
		elseif c_ops then -- Garry's Mod operators support
			if consume("^&&") then
				return Token.new(TokenVariant.Operator, "&&")
			elseif consume("^||") then
				return Token.new(TokenVariant.Operator, "||")
			elseif consume("^!") then
				return Token.new(TokenVariant.Operator, "!")
			end
		end

		if consume("^%.%.%.") then
			return Token.new(TokenVariant.Vararg, nil)
		elseif consume("^%.%.") then
			return Token.new(TokenVariant.Operator, "..")
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

	Table = 35, Literal = 36, Lambda = 37, Identifier = 38
}

Node.Variant = NodeVariant

---@alias Scope { labels: table<string, boolean>, pending_gotos: table<string, Token> }

---@param tokens Token[]
---@param version Version?
local function parse(tokens, version)
	version = version or _VERSION

	local has_continue, c_ops = version == "Garry's Mod", version == "Garry's Mod"

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

	---@overload fun(variant: TokenVariant): Token?
	---@generic T
	---@param variant TokenVariant
	---@param data T
	local function consume(variant, data) ---@return Token<T>?
		local p = tokens[index]
		if p and p.variant == variant then
			if data ~= nil and p.data ~= data then
				return
			end
			index = index + 1
			return p
		end
	end

	local function peek(variant --[[@param variant TokenVariant]])
		return tokens[index] and tokens[index].variant == variant
	end

	local arguments, parameters, stmt, expr, block, varlist, explist
	local function prim()
		for _, variant in ipairs {
			TokenVariant.Nil, TokenVariant.Boolean,
			TokenVariant.Integer, TokenVariant.Hexadecimal, TokenVariant.Binary, TokenVariant.Decimal,
			TokenVariant.MString, TokenVariant.String, TokenVariant.Vararg
		} do
			local val = consume(variant)
			if val then
				return Node.new(NodeVariant.Literal, { variant, val })
			end
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
			if consume(TokenVariant.Grammar, "}") then return Node.new(NodeVariant.Table, {}) end
			local fields = {}
			repeat
				fields[#fields + 1] = field()
				fieldsep()
			until consume(TokenVariant.Grammar, "}")
			return Node.new(NodeVariant.Table, fields)
		end
	end

	local function namelist(msg --[[@param msg string?]]) ---@return Token[]
		local idents = {}
		repeat
			idents[#idents + 1] = assert(consume(TokenVariant.Identifier), "Expected identifier for " .. (msg or "parameter"))
		until not consume(TokenVariant.Grammar, ",")
		return idents
	end

	local function attnamelist(msg --[[@param msg string?]]) ---@return { [1]: Token, [2]: Token? }[]
		local out = {}
		repeat
			out[#out + 1] = { assert(consume(TokenVariant.Identifier), "Expected identifier for " .. (msg or "parameter")), consume(TokenVariant.Attribute)}
		until not consume(TokenVariant.Grammar, ",")
		return out
	end

	local declnamelist = version >= "Lua 5.4" and attnamelist or namelist

	function expr() -- nil | boolean | number | string | ... | function | prefixexp | tableconstructor | exp binop exp | unop exp
		local p = prim() or prefixexp() or tableconstructor()

		if not p then
			if consume(TokenVariant.Operator, "not") or (c_ops and consume(TokenVariant.Operator, "!")) then
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
			elseif p then
				local str = consume(TokenVariant.String) or consume(TokenVariant.MString)
				if str then
					p = Node.new(NodeVariant.Call, { p, { Node.new(NodeVariant.Literal, { str.variant, str }) } })
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

		for op, variant in pairs {
			["+"] = NodeVariant.Addition, ["-"] = NodeVariant.Subtraction,
			["*"] = NodeVariant.Multiply, ["/"] = NodeVariant.Divide,
			["%"] = NodeVariant.Modulus, ["^"] = NodeVariant.Pow,
			[".."] = NodeVariant.Concat, ["or"] = NodeVariant.Or,
			["and"] = NodeVariant.And, ["<"] = NodeVariant.LessThan,
			["<="] = NodeVariant.LessThanEq, [">"] = NodeVariant.GreaterThan,
			[">="] = NodeVariant.GreaterThanEq, ["=="] = NodeVariant.Equals,
			["~="] = NodeVariant.NotEquals, ["!="] = NodeVariant.NotEquals,
			["||"] = NodeVariant.Or, ["&&"] = NodeVariant.And -- Yes, C operators are included, but I'm assuming people will properly pass tokens with the same version.
		} do
			if consume(TokenVariant.Operator, op) then
				return Node.new(variant, { p, expr() })
			end
		end

		return p
	end


	function block(ending --[[@param ending string]]) ---@return Node[]?
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

	function arguments(msg --[[@param msg string?]], ending --[[@param ending string?]]) ---@return Node[]
		local exprs = {}
		if ending and consume(TokenVariant.Grammar, ending) then return exprs end
		repeat
			exprs[#exprs + 1] = assert(expr(), "Expected expression for " .. (msg or "arguments"))
			if ending and consume(TokenVariant.Grammar, ending) then return exprs end
		until not consume(TokenVariant.Grammar, ",")
		assert(not ending or consume(TokenVariant.Grammar, ending), "Expected " .. ending .. " to complete arguments")
		return exprs
	end

	function explist() ---@return Node[]?
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

	--- `Name` | `prefixexp "[" exp "]"` | `prefixexp "." Name`
	local function var() ---@return { [1]: Node, [2]: Node[] }?
		local ident = prefixexp()
		if not ident then return end

		local chain, i = {}, 1 ---@type Node[]
		while true do
			if consume(TokenVariant.Grammar, "[") then
				local key = assert(expr(), "Expected expression for index key")
				assert(consume(TokenVariant.Grammar, "]"), "Expected ] to end index")
				chain[i], i = key, i + 1
			elseif consume(TokenVariant.Grammar, ".") then
				chain[i], i = Node.new(NodeVariant.Literal, {TokenVariant.String, assert(consume(TokenVariant.Identifier), "Expected identifier for dot index")}), i + 1
			else
				break
			end
		end

		return { ident, chain }
	end

	function varlist(msg --[[@param msg string]]) ---@return { [1]: Node, [2]: Node? }[]
		local vars = {}
		repeat
			vars[#vars + 1] = assert(var(), "Expected identifier for " .. msg)
		until not consume(TokenVariant.Grammar, ",")
		return vars
	end

	function parameters() ---@return Token[]?
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
		elseif consume(TokenVariant.Keyword, "if") then
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
		elseif has_continue and consume(TokenVariant.Identifier, "continue") then
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
				local idents = declnamelist("local declaration")
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
		nodes[#nodes + 1] = assert(stmt(), "Failed to parse: '" .. tostring(tokens[index]) .. "'")
		while consume(TokenVariant.Grammar, ";") do end
	end
	popScope()

	return Node.new(NodeVariant.Chunk, nodes)
end

--#endregion

return {
	tokenize = tokenize,
	parse = parse,

	Token = Token,
	Node = Node
}