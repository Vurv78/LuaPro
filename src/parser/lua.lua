local TOKEN_KINDS = require("lexer/lua").Kinds

---@enum NodeKind
local KINDS = {
	Comment = 0, -- Multiline or single comments

	While = 1, -- while true do end
	For = 2, -- for i = 1, 10 do end
	If = 3, -- if x then elseif y then else end
	Repeat = 4, -- repeat until xyz
	Function = 5, -- function foo() end
	Block = 6, -- do end

	LVarDecl = 7, -- local v = ...
	VarAssign = 8, -- v = ...

	Escape = 9, -- break, goto, return
	Label = 10, -- ::label::

	GroupedExpr = 11, -- (...)
	BinaryOps = 12, -- Operators that take two operands.
	UnaryOps = 13, -- not, -
	Index = 14, -- . or [] indexing
	Call = 15, -- foo()
	MetaCall = 16, -- foo:bar()

	Lambda = 17, -- function() end
	Table = 18, -- {}
	Literal = 19, -- Number, bool, nil, string
	Identifier = 20, -- foo, bar, _foo, _bar
}

local KINDS_INV = {}
for k, v in pairs(KINDS) do
	KINDS_INV[v] = k
end

local Statements
local Expressions

---@class Node
---@field kind NodeKind
---@field data table
local Node = {}
Node.__index = Node

function Node:__tostring()
	return string.format("Node \"%s\" (#%u)", KINDS_INV[self.kind], #self.data)
end

---@param kind NodeKind
---@param data table
function Node.new(kind, data)
	return setmetatable({kind = kind, data = data}, Node)
end

---@class Parser
---@field nodes table<number, Node>
---@field node_idx integer
---@field tok_idx integer
local Parser = {}
Parser.__index = Parser

function Parser:reset()
	self.nodes = {}
	self.node_idx = 0
	self.tok_idx = 0

	return self
end

---@return Parser
function Parser.new()
	---@type Parser
	return setmetatable({}, Parser):reset()
end

--- Parses a stream of tokens into an abstract syntax tree (AST)
---@param tokens table<number, Token> # Tokens retrieved from the [Lexer]
---@return table<number, Node>
function Parser:parse(tokens)
	assert(type(tokens) == "table", "bad argument #1 to 'Parser:parse' (table expected, got " .. type(tokens) .. ")")
	self.tokens = tokens

	local ok, res = pcall(self.root, self)
	if not ok then
		local tok = self.tokens[self.tok_idx]
		local msg = string.format("Parser error: [%q] at line %u, cols %u:%u", res, tok.startline, tok.startcol, tok.endcol)
		error(msg, 0)
	end

	return res
end

function Parser:hasTokens()
	return self.tok_idx < #self.tokens
end

function Parser:root()
	local nodes = {}
	self.nodes = nodes

	repeat
		local node = self:next()
		nodes[#nodes + 1] = node
	until not node

	return nodes
end

---@return Node?
function Parser:next()
	if not self:hasTokens() then
		return nil
	end

	local tok = self:nextToken()

	local node = self:parseStatement(tok)
	if not node then
		local n = self:parseExpression(tok)
		if n then
			assert(n.kind == KINDS.Call or n.kind == KINDS.MetaCall, "Can only have call expressions at the top level. Got " .. KINDS_INV[n.kind])
			node = n
		end
	end

	if node then
		while self:popToken( TOKEN_KINDS.Grammar, ";" ) do
			-- Dump all ;'s. Idk why lua allows this but whatever
			local _ = nil -- shut up gluafixer
		end

		return node
	elseif tok then
		error( string.format("Unexpected %s at line %u, chars [%u-%u]", tok, tok.startline, tok.startcol, tok.endcol) )
	else
		error( "Unexpected end of file" )
	end
end

--- Shifts the parser to the next token.
--- Not to be confused with [Parser:next()], which tries to create another node
---@return Token?
function Parser:nextToken()
	self.tok_idx = self.tok_idx + 1
	return self.tokens[ self.tok_idx ]
end

--- Inverse of [Parser:nextToken()].
function Parser:prevToken()
	self.tok_idx = self.tok_idx - 1
	return self.tokens[ self.tok_idx ]
end

---@return Token?
function Parser:peek()
	return self.tokens[ self.tok_idx + 1 ]
end

---@return Token?
function Parser:peekBack()
	return self.tokens[ self.tok_idx - 1 ]
end

---@return Node?
function Parser:lastNode()
	return self.nodes[ self.node_idx ]
end

--- Returns the last node with the given metadata (assuming it exists and fits the given kind and value)
---@param kind NodeKind
---@return Node
function Parser:lastNodeWith(kind)
	local last = self:lastNode()
	if last then
		return last.kind == kind
	end
end

--- Returns the last node with the given metadata (assuming it exists and fits the given kind and value)
---@param kind NodeKind[]
---@return boolean
function Parser:lastNodeAnyOfKind(kind)
	local last = self:lastNode()
	if not last then return false end

	for _, k in ipairs(kind) do
		if last.kind == k then
			return true
		end
	end
	return false
end

--- Returns if a given token is of kind 'kind' and has an inner value
---@generic T: Token
---@param token Token
---@param kind T
---@param value string? # Optional value to match against
---@return boolean
local function isToken(token, kind, value)
	if not token then return false end

	if value ~= nil then
		return token.kind == kind and token.raw == value
	else
		return token.kind == kind
	end
end

--- Like isToken, but accepts an array of values rather than just one
---@generic T: Token
---@param token Token?
---@param kind T
---@param values table<number, string>
---@return string # The raw value from 'values' that matched.
local function isAnyOf(token, kind, values)
	-- TODO: Maybe we shouldn't check for nil here.
	if not token or token.kind ~= kind then return false end

	for _, val in ipairs(values) do
		if token.raw == val then return val end
	end
end

--- Like isAnyOf, but for the kind instead of values.
---@param token Token?
---@param kinds table<number, Token>
---@return Token? # The [TokenKinds] that matched.
local function isAnyOfKind(token, kinds)
	for _, kind in ipairs(kinds) do
		if isToken(token, kind, nil) then return kind end
	end
end

--- Like isToken, but peeks ahead, uses that as the token, skips if it matches it.
---@generic T: Token
---@param kind T
---@param value string? # Optional value to match against
---@return T? # The token that matched
function Parser:popToken(kind, value)
	local token = self:peek()
	if isToken(token, kind, value) then
		return self:nextToken()
	end
end

--- Same as isAnyOf, but skips if it matches it.
---@generic T: Token
---@param kind T
---@param values table<number, string> # Values to match against
---@return string? # Raw value that matched from 'values'
function Parser:popAnyOf(kind, values)
	local ret = isAnyOf(self:peek(), kind, values)
	if ret then
		self.tok_idx = self.tok_idx + 1
		return ret
	end
end

--- Accepts a block, or throws an error if it couldn't.
---@param kind "do|then|else"
---@param endings boolean|table<number, "else|elseif|end|until">?
---@return table<number, Node>
---@return string? # The ending that was used
function Parser:acceptBlock(kind, endings)
	if kind then
		assert( self:popToken(TOKEN_KINDS.Keyword, kind), "Expected '" .. kind .. "' to start block" )
	end

	local nodes, node_idx = {}, 0
	endings = endings or { "end" }


	-- Empty block
	local pop = self:popAnyOf(TOKEN_KINDS.Keyword, endings)
	if pop then
		return nodes, pop
	end

	repeat
		node_idx = node_idx + 1
		local node = self:next()
		nodes[node_idx] = node

		pop = self:popAnyOf(TOKEN_KINDS.Keyword, endings)

		if pop then
			return nodes, pop
		end
	until not node

	error("Ending keyword (" .. table.concat(endings, " or ") .. ") missing, to close block")
end

---@return string
function Parser:acceptIdent()
	---@type IdentifierToken
	local tok = self:popToken(TOKEN_KINDS.Identifier)
	if tok then
		return tok.raw
	elseif self:popToken(TOKEN_KINDS.Keyword) then
		error("Expected identifier, got keyword")
	end
end

---@param tok Token
function Parser:parseExpression( tok )
	return self:parseExpression1( self:parsePrimary( tok ), 0 )
end

---@param tok Token
---@return Node
function Parser:parsePrimary(tok)
	return Expressions[1](self, tok)
end

---@param lhs Node
---@param min_precedence integer
---@return Node?
function Parser:parseExpression1(lhs, min_precedence)
	local lookahead = self:peek()

	--- Op String, Precedence, Is Unary
	---@type { [1]: string, [2]: number, [3]: boolean }
	local dat = lookahead and lookahead.data
	while isToken(lookahead, TOKEN_KINDS.Operator) and dat[2] ~= -1 and dat[2] >= min_precedence do
		local op = self:nextToken() -- Consume lookahead
		local rhs = self:parsePrimary(self:nextToken())
		lookahead = self:peek()

		while isToken(lookahead, TOKEN_KINDS.Operator) and lookahead.data[2] ~= -1 and lookahead.data[2] > dat[2] do
			rhs = self:parseExpression1(rhs, lookahead.data[2] + 1)
			lookahead = self:peek()
		end
		lhs = Node.new(KINDS.BinaryOps, { op.data[1], lhs, rhs })
	end

	return lhs
end

---@param tok Token
---@return Node?
function Parser:parseStatement(tok)
	for kind, handler in pairs(Statements) do
		local data = handler(self, tok)
		if data then
			return Node.new(kind, data)
		end
	end
end

---@return Node?
function Parser:acceptExpression()
	return self:parseExpression(self:nextToken())
end

---@return table<number, string>
function Parser:acceptParameters()
	if not self:popToken(TOKEN_KINDS.Grammar, "(") then return end
	local args = {}
	if self:popToken(TOKEN_KINDS.Grammar, ")") then return args end

	local arg
	while self:hasTokens() do
		arg = self:acceptIdent()
		if not arg then break end

		args[#args + 1] = arg

		if not self:popToken(TOKEN_KINDS.Grammar, ",") then
			assert( self:popToken(TOKEN_KINDS.Grammar, ")"), "Expected ) to end function parameters" )
			return args
		end
	end
end

-- Accept comma separated list of expressions (5, 2, nil, "Hello")
---@param noparenthesis boolean
---@return table<number, Node>
function Parser:acceptArguments( noparenthesis )
	if noparenthesis or self:popToken(TOKEN_KINDS.Grammar, "(") then
		local nargs, args = 1, {}

		local arg = self:parseExpression(self:nextToken())
		if noparenthesis and not arg then
			-- No expression found, no arguments.
			self:prevToken()
		end

		while arg do
			args[nargs] = arg
			nargs = nargs + 1

			if not noparenthesis and self:popToken(TOKEN_KINDS.Grammar, ")") then
				break
			end

			if self:popToken(TOKEN_KINDS.Grammar, ",") then
				arg = assert( self:parseExpression(self:nextToken()), "Expected expression after comma" )
			else
				assert( noparenthesis or self:popToken(TOKEN_KINDS.Grammar, ")"), "Expected ) or , after argument in call expr" )
				break
			end
		end

		return args
	end
end

--- Tries to accept an indexing operation e.g. `.b` or `[1]`
---@return string kind
---@return Node|string|nil index
function Parser:acceptIndex()
	if self:popToken(TOKEN_KINDS.Grammar, ".") then
		-- Ident index
		local index = assert( self:acceptIdent(), "Expected identifier after '.'" )
		return ".", index
	elseif self:popToken(TOKEN_KINDS.Grammar, "[") then
		local index = assert( self:acceptExpression(), "Expected expression after '['" )
		assert( self:popToken(TOKEN_KINDS.Grammar, "]"), "Expected ']' after expression" )

		return "[]", index
	end
end

Statements = {
	--- Shouldn't be a part of 'Statements' but w/e
	---@param self Parser
	---@param token Token
	[KINDS.Comment] = function(self, token)
		if isToken(token, TOKEN_KINDS.Comment) then
			return { false, token.value }
		elseif isToken(token, TOKEN_KINDS.MComment) then
			return { true, token.value, token.data[1] }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.While] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "while") then
			local cond = assert(self:acceptExpression(), "Expected condition after 'while'")

			local block = self:acceptBlock("do")
			return { cond, block }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.If] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "if") then
			local cond = assert( self:acceptExpression(), "Expected condition after 'if'")
			local block, ty = self:acceptBlock("then", {"else", "end", "elseif"})

			local conditions = { {cond, block} }


			-- Return a table of this sort of format:
			-- { [1]: table<number, {Node, table<number, Node>}>, [2]: table<number, Node> }
			-- Where [1] is a table of conditions and blocks corresponding to the original 'if' block and 'elseif' chains.
			-- [2] is the 'else' case, if specified

			while ty ~= "end" do
				if ty == "elseif" then
					cond = assert( self:acceptExpression(), "Expected condition after 'elseif'" )
					block, ty = self:acceptBlock("then", {"else", "end", "elseif"})
					conditions[#conditions + 1] = { cond, block }
				else
					local else_block = self:acceptBlock(nil, {"end"})
					return { conditions, else_block }
				end
			end

			return { conditions, nil }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.For] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "for") then
			local ident = assert( self:acceptIdent(), "Expected identifier after 'for'" )
			assert( self:popToken(TOKEN_KINDS.Operator, "="), "Expected '=' after identifier" )
			local start = assert( self:acceptExpression(), "Expected loop start expression after '='" )
			assert( self:popToken(TOKEN_KINDS.Grammar, ","), "Expected ',' after start expression" )
			local end_ = assert( self:acceptExpression(), "Expected loop end expression after ','" )

			local step
			if self:popToken(TOKEN_KINDS.Grammar, ",") then
				step = assert( self:acceptExpression(), "Expected loop step expression after ','" )
			end

			local block = assert( self:acceptBlock("do"), "Expected block after for loop setup" )
			return { ident, start, end_, step, block }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Function] = function(self, token)
		local is_local = false
		if isToken(token, TOKEN_KINDS.Keyword, "local") then
			if not self:popToken(TOKEN_KINDS.Keyword, "function") then
				-- Not a function. Local var
				return
			end
			is_local = true
		elseif not isToken(token, TOKEN_KINDS.Keyword, "function") then
			-- Not a function
			return
		end

		local tbl
		local name = assert( self:acceptIdent(), "Expected identifier after 'function'" )
		local idkind = self:popAnyOf(TOKEN_KINDS.Grammar, {".", ":"})
		if idkind then
			tbl = name
			name = assert( self:acceptIdent(), "Expected identifier after '.' or ':'" )
		end

		local args = self:acceptParameters()
		local block = assert( self:acceptBlock(nil, {"end"}), "Expected block after function" )
		return { is_local, tbl, idkind, name, args, block }
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Repeat] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "repeat") then
			local block = assert( self:acceptBlock(nil, {"until"}), "Expected block after 'repeat'" )
			local cond = assert( self:acceptExpression(), "Expected condition after 'until'" )
			return { cond, block }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Block] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "do") then
			local block = self:acceptBlock(nil, {"end"})
			return { block }
		end
	end,

	--- local a(, b, c) (= 1(, 2, 3))
	--- local a
	--- local a, b, c
	--- local a = 5
	--- local a, b, c = 1, 2, 3
	---@param self Parser
	---@param token Token
	[KINDS.LVarDecl] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "local") then
			local next = self:peek()
			if next and next.kind == TOKEN_KINDS.Identifier then -- Ugly way to avoid changing my while logic a few lines down :/
				local names = {}

				while true do
					local name = assert( self:acceptIdent(), "Expected identifier after 'local'" )
					names[#names + 1] = name

					if not self:popToken(TOKEN_KINDS.Grammar, ",") then break end
				end

				if self:popToken(TOKEN_KINDS.Operator, "=") then
					local exprs = self:acceptArguments(true)
					return { names, exprs }
				else
					return { names }
				end
			end
		end
	end,

	--- local a(, b, c) (= 1(, 2, 3))
	--- local a
	--- local a, b, c
	--- local a = 5
	--- local a, b, c = 1, 2, 3
	---@param self Parser
	---@param token Token
	[KINDS.VarAssign] = function(self, token)
		if isToken(token, TOKEN_KINDS.Identifier) then
			local idents = {}
			self:prevToken()

			while true do
				local ident = assert( self:popToken(TOKEN_KINDS.Identifier), "Expected identifier in assignment" )
				local kind, idx = self:acceptIndex()

				local ident_node = Node.new(KINDS.Identifier, {ident.raw})

				if kind then
					idents[#idents + 1] = Node.new(KINDS.Index, {kind, ident_node, idx})
				else
					idents[#idents + 1] = ident_node
				end

				if not self:popToken(TOKEN_KINDS.Grammar, ",") then break end
			end

			if self:popToken(TOKEN_KINDS.Operator, "=") then
				local exprs = self:acceptArguments(true)
				return { idents, exprs }
			else
				-- Only names were given. Probably an expr?
				return
			end
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Escape] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "return") then
			local exprs = self:acceptArguments(true)
			return { "return", exprs }
		elseif isToken(token, TOKEN_KINDS.Keyword, "goto") then
			local label = assert( self:acceptIdent(), "Expected label name after 'goto'" )
			return { "goto", label }
		elseif isToken(token, TOKEN_KINDS.Keyword, "break") then
			return { "break" }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Label] = function(self, token)
		if isToken(token, TOKEN_KINDS.Label) then
			return { token.value }
		end
	end
}

Expressions = {
	--- Unary Ops
	[1] = function(self, token)
		if isToken(token, TOKEN_KINDS.Operator, "-") then
			local exp = Expressions[1](self, self:nextToken())
			return Node.new(KINDS.UnaryOps, { "-", exp })
		elseif isToken(token, TOKEN_KINDS.Operator, "not") then
			local exp = Expressions[1](self, self:nextToken())
			return Node.new(KINDS.UnaryOps, { "not", exp })
		end
		return Expressions[2](self, token)
	end,

	--- Binary Ops
	---@param self Parser
	---@param token Token
	[2] = function(self, token)
		return Expressions[3](self, token)
	end,

	--- Call Expr
	---@param self Parser
	---@param token Token
	[3] = function(self, token)
		local expr = Expressions[4](self, token)
		local args = self:acceptArguments(false)

		if args then
			return Node.new(KINDS.Call, {expr, args})
		else
			local str = self:popToken(TOKEN_KINDS.String)
			if str then
				local string_node = Node.new(KINDS.Literal, {"string", str.raw, str.value})
				return Node.new(KINDS.Call, {expr, {string_node}})
			elseif self:popToken(TOKEN_KINDS.Grammar, "{") then
				-- Don't need a self:prevToken() here as calling Expressions[4] will not consume the { token itself.
				-- Probably want a self:currentToken() though :p
				local tok = self.tokens[self.tok_idx]

				local tbl = assert( Expressions[4](self, tok), "Expected table for table literal call after '{'" )
				return Node.new(KINDS.Call, {expr, {tbl}})
			end
		end

		return expr
	end,

	--- Index (.foo or ["foo"])
	---@param self Parser
	---@param token Token
	[4] = function(self, token)
		local expr = Expressions[5](self, token)

		local idx = self:acceptIndex(expr)
		if idx then return idx end

		return expr
	end,

	--- Table literal
	---@param self Parser
	---@param token Token
	[5] = function(self, token)
		if isToken(token, TOKEN_KINDS.Grammar, "{") then
			local entries = {}

			if self:popToken(TOKEN_KINDS.Grammar, "}") then
				-- Empty table
				return Node.new(KINDS.Table, {entries})
			end

			local arr_index, skip = 1, false
			while true do
				local key
				if self:popToken(TOKEN_KINDS.Grammar, "[") then
					key = assert( self:acceptExpression(), "Expected expression after '[' for table key")
					assert( self:popToken(TOKEN_KINDS.Grammar, "]"), "Expected ']' after table key" )
				elseif self:acceptIdent() then
					key = self.tokens[ self.tok_idx ].value
				else
					-- Implicit / array part key
					local val = self:acceptExpression()

					if val then
						entries[ #entries + 1 ] = { arr_index, val }
						arr_index = arr_index + 1

						-- Would love a 'continue' here lua..
						skip = true
					else
						break
					end
				end

				if not skip then
					assert( self:popToken(TOKEN_KINDS.Operator, "="), "Expected '=' after key" )
					local value = assert( self:acceptExpression(), "Expected value after '='" )
					entries[#entries + 1] = { key, value }
				else
					skip = false
				end

				if not self:popToken(TOKEN_KINDS.Grammar, ",") then
					break
				end
			end

			assert( self:popToken(TOKEN_KINDS.Grammar, "}"), "Expected '}' after table literal" )
			return Node.new(KINDS.Table, {entries})
		end

		return Expressions[6](self, token)
	end,

	--- Lambda
	---@param self Parser
	---@param token Token
	[6] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "function") then
			local params = self:acceptParameters()
			local body = self:acceptBlock(nil, {"end"})

			return Node.new(KINDS.Lambda, {params, body})
		end

		return Expressions[7](self, token)
	end,

	--- Literals
	---@param self Parser
	---@param token Token
	[7] = function(self, token)
		if isAnyOfKind(token, {TOKEN_KINDS.Integer, TOKEN_KINDS.Binary, TOKEN_KINDS.Hexadecimal, TOKEN_KINDS.Decimal}) then
			return Node.new( KINDS.Literal, {"number", token.raw, token.value, token.kind} )
		elseif isToken(token, TOKEN_KINDS.Boolean) then
			return Node.new( KINDS.Literal, {"boolean", token.raw, token.raw == "true"} )
		elseif isToken(token, TOKEN_KINDS.Keyword, "nil") then
			return Node.new( KINDS.Literal, {"nil", token.raw, nil} )
		elseif isAnyOfKind(token, {TOKEN_KINDS.String, TOKEN_KINDS.MString}) then
			return Node.new( KINDS.Literal, {"string", token.raw, token.value} )
		end
		return Expressions[8](self, token)
	end,

	--- Ident / Variables
	---@param self Parser
	---@param token Token
	[8] = function(self, token)
		if isToken(token, TOKEN_KINDS.Identifier) then
			return Node.new( KINDS.Identifier, {token.raw} )
		end
		return Expressions[9](self, token)
	end,

	--- Grouped Expr
	---@param self Parser
	---@param token Token
	[9] = function(self, token)
		if isToken(token, TOKEN_KINDS.Grammar, "(") then
			local expr = assert( self:acceptExpression(), "Expected expression after '('" )
			assert( self:popToken(TOKEN_KINDS.Grammar, ")"), "Expected ')' after expression" )

			return Node.new( KINDS.GroupedExpr, {expr} )
		end
	end
}

Parser.Kinds = KINDS
Parser.Node = Node

return Parser