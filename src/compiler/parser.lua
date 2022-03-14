local TOKEN_KINDS = require("compiler/lexer").Kinds

---@class NodeKinds
local KINDS = {
	Comment = 0,

	While = 1,
	For = 2,
	If = 3,
	Elseif = 4,
	Else = 5,
	Repeat = 6,
	Function = 7,

	LVarDecl = 8, -- local v = ...
	VarAssign = 9, -- v = ...

	ArithmeticOps = 10, -- + - / % *
	LogicalOps = 11, -- or, and
	UnaryOps = 12, -- not, -
	Index = 13, -- . or [] indexing
	Call = 14, -- foo()
	MetaCall = 15, -- foo:bar()

	Table = 16, -- {}
	Literal = 17, -- Number, bool, nil, string
	Ident = 18, -- foo, bar, _foo, _bar
}

local KINDS_INV = {}
for k, v in pairs(KINDS) do
	KINDS_INV[v] = k
end

local Statements
local Expressions

---@class Node
---@field kind NodeKinds
---@field data table
local Node = {}
Node.__index = Node

function Node:__tostring()
	return string.format("Node \"%s\" (#%u)", KINDS_INV[self.kind], #self.data)
end

---@param kind NodeKinds
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

---@return Parser
function Parser.new()
	---@type Parser
	return setmetatable({
		tok_idx = 0,
		nodes = {},
		node_idx = 0
	}, Parser)
end

--- Parses a stream of tokens into an abstract syntax tree (AST)
---@param tokens table<number, Token> # Tokens retrieved from the [Tokenizer]
---@return table<number, Node>
function Parser:parse(tokens)
	assert(type(tokens) == "table", "bad argument #1 to 'Parser:parse' (table expected, got " .. type(tokens) .. ")")
	self.tokens = tokens

	local ok, res = pcall(self.root, self)
	if not ok then
		-- local tok = self.tokens[self.tok_idx]
		error("Parser error: [" .. res .. "]", 0)
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
	local node = self:parseStatement(tok) or self:parseExpression(tok)
	if node then
		return node
	else
		error("Unexpected token " .. tok.kind .. " '" .. tok.raw .. "'")
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
---@param kind NodeKinds
---@param value string?
---@return Node
function Parser:lastNodeWith(kind, value)
	local last = self:lastNode()
	if not last then return end

	if value ~= nil  then
		return (last.kind == kind and last.raw == value) and last
	else
		return (last.kind == kind) and last
	end
end

--- Returns the last node with the given metadata (assuming it exists and fits the given kind and value)
---@param kind table<number, NodeKinds>
---@return boolean
function Parser:lastNodeAnyOfKind(kind)
	local last = self:lastNode()
	if not last then return end

	for _, k in ipairs(kind) do
		if last.kind == k then
			return true
		end
	end
	return false
end

--- Returns if a given token is of kind 'kind' and has an inner value
---@param token Token
---@param kind TokenKinds
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
---@param token Token?
---@param kind TokenKinds
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
---@param kinds table<number, TokenKinds>
---@return TokenKinds? # The [TokenKinds] that matched.
local function isAnyOfKind(token, kinds)
	for _, kind in ipairs(kinds) do
		if isToken(token, kind, nil) then return kind end
	end
end

--- Like isToken, but peeks ahead, uses that as the token, skips if it matches it.
---@param kind TokenKinds
---@param value string? # Optional value to match against
---@return Token? # The token that matched
function Parser:popToken(kind, value)
	local token = self:peek()
	if isToken(token, kind, value) then
		return self:nextToken()
	end
end

--- Same as isAnyOf, but skips if it matches it.
---@param kind TokenKinds
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
function Parser:acceptBlock(kind, endings)
	if kind then
		assert( self:popToken(TOKEN_KINDS.Keyword, kind), "Expected '" .. kind .. "' to start block" )
	end

	local nodes, node_idx = {}, 0
	endings = endings or { "end" }

	-- Empty block
	if self:popAnyOf(TOKEN_KINDS.Keyword, endings) then
		return nodes
	end

	repeat
		node_idx = node_idx + 1
		local node = self:next()
		nodes[node_idx] = node

		if self:popAnyOf(TOKEN_KINDS.Keyword, endings) then
			return nodes
		end
	until not node

	error("Ending keyword (" .. table.concat(endings, " or ") .. ") missing, to close block")
end

---@return string
function Parser:acceptIdent()
	local tok = self:popToken(TOKEN_KINDS.Ident)
	return tok and tok.raw
end

---@param tok Token
---@return Node?
function Parser:parseExpression(tok)
	return Expressions[1](self, tok)
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

function Parser:acceptParameters()
	if not self:popToken(TOKEN_KINDS.Grammar, "(") then return end
	local args = {}
	if self:popToken(TOKEN_KINDS.Grammar, ")") then return args end

	local arg, ty
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
		while arg do
			print(arg, nargs)
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

Statements = {
	--- Shouldn't be a part of 'Statements' but w/e
	---@param self Parser
	---@param token Token
	[KINDS.Comment] = function(self, token)
		if isToken(token, TOKEN_KINDS.Comment) then
			return { false, token.val }
		elseif isToken(token, TOKEN_KINDS.MComment) then
			return { true, token.val, token.data[1] }
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

			local block = self:acceptBlock("then", {"else", "end", "elseif"})
			return { cond, block }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Elseif] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "elseif") then
			assert( self:lastNodeWith(KINDS.If), "Expected if statement before elseif" )
			local cond = assert( self:acceptExpression(), "Expected condition after 'elseif'" )
			local block = assert( self:acceptBlock("then", {"end", "else", "elseif"}), "Expected block after 'elseif'" )

			return { cond, block }
		end
	end,

	---@param self Parser
	---@param token Token
	[KINDS.Else] = function(self, token)
		if isToken(token, TOKEN_KINDS.Keyword, "else") then
			assert( self:lastNodeAnyOfKind({KINDS.If, KINDS.Elseif}), "Expected if or elseif statement before else" )

			self:prevToken() -- Go back so that we can use acceptBlock with 'else' as a starting block bracket.
			local block = assert( self:acceptBlock("else"), "Expected block after 'else'" )

			return { block }
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
			if next and next.kind == TOKEN_KINDS.Ident then -- Ugly way to avoid changing my while logic a few lines down :/
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
}

Expressions = {
	--- Arithmetic
	---@param self Parser
	---@param token Token
	[1] = function(self, token)
		return Expressions[2](self, token)
	end,

	--- Call Expr
	---@param self Parser
	---@param token Token
	[2] = function(self, token)
		local expr = Expressions[3](self, token)
		local args = self:acceptArguments()

		if args then
			return Node.new(KINDS.Call, {expr, args})
		end

		return expr
	end,

	--- Index (.foo or ["foo"])
	---@param self Parser
	---@param token Token
	[3] = function(self, token)
		local expr = Expressions[4](self, token)
		if self:popToken(TOKEN_KINDS.Grammar, ".") then
			-- Ident index
			local index = assert( self:acceptIdent(), "Expected identifier after '.'" )
			return Node.new(KINDS.Index, {".", expr, index})
		elseif self:popToken(TOKEN_KINDS.Grammar, "[") then
			local index = assert( self:acceptExpression(), "Expected expression after '['" )
			assert( self:popToken(TOKEN_KINDS.Grammar, "]"), "Expected ']' after expression" )
			return Node.new(KINDS.Index, {"[]", expr, index})
		end

		return expr
	end,

	--- Table literal
	---@param self Parser
	---@param token Token
	[4] = function(self, token)
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
					key = self.tokens[ self.tok_idx ].val
					print("the ident", key)
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

		return Expressions[5](self, token)
	end,

	--- Literals
	---@param self Parser
	---@param token Token
	[5] = function(self, token)
		if isToken(token, TOKEN_KINDS.Number) then
			return Node.new( KINDS.Literal, {"number", token.raw, tonumber(token.raw)} )
		elseif isToken(token, TOKEN_KINDS.Boolean) then
			return Node.new( KINDS.Literal, {"boolean", token.raw, token.raw == "true"} )
		elseif isToken(token, TOKEN_KINDS.Keyword, "nil") then
			return Node.new( KINDS.Literal, {"nil", token.raw, nil} )
		elseif isToken(token, TOKEN_KINDS.String) then
			return Node.new( KINDS.Literal, {"string", token.raw, token.val} )
		end
		return Expressions[6](self, token)
	end,

	--- Ident / Variables
	---@param self Parser
	---@param token Token
	[6] = function(self, token)
		if isToken(token, TOKEN_KINDS.Ident) then
			return Node.new( KINDS.Ident, {token.raw} )
		end
	end
}

Parser.Kinds = KINDS

return Parser