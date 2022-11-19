---@enum TokenKind
local TokenKind = {
	Whitespace = 1,
	Comment = 2,
	Keyword = 3,

	Label = 4,

	Hexadecimal = 5,
	Binary = 6,
	Decimal = 7,
	Integer = 8,
	String = 9,
	Boolean = 10,

	Operator = 11,
	Grammar = 12,
	Ident = 13
}

---@enum GrammarKind
local GrammarKind = {
	-- {
	LeftCurly = 1,
	-- }
	RightCurly = 2,
	-- (
	LeftParen = 3,
	-- )
	RightParen = 4,
	-- [
	LeftSquare = 5,
	-- ]
	RightSquare = 6,
	-- :
	Colon = 7,
	-- ;
	Semicolon = 8,
	-- ,
	Comma = 9,
	-- .
	Dot = 10,
	-- ...
	Vararg = 11
}

local Keywords = {
	["while"] = true, ["do"] = true, ["if"] = true, ["repeat"] = true,
	["else"] = true, ["elseif"] = true, ["until"] = true, ["then"] = true,
	["end"] = true, ["for"] = true, ["in"] = true, ["local"] = true,
	["goto"] = true, ["break"] = true, ["return"] = true, ["function"] = true,

	["nil"] = true, ["false"] = true, ["true"] = true
}

local Grammar = {
	["{"] = GrammarKind.LeftCurly, ["}"] = GrammarKind.RightCurly, [","] = GrammarKind.Comma, ["["] = GrammarKind.LeftSquare,
	["]"] = GrammarKind.RightSquare, [":"] = GrammarKind.Colon, [";"] = GrammarKind.Semicolon, ["("] = GrammarKind.LeftParen,
	[")"] = GrammarKind.RightParen, ["."] = GrammarKind.Dot, ["..."] = GrammarKind.Vararg
}

local ValidEscapes = {
	["b"] = "\b",
	["n"] = "\n",
	["r"] = "\r",
	["a"] = "\a",
	["'"] = "'",
	['"'] = '"',
}

---@class Lexer
---@field source string
---@field ptr integer # Position in the [self.source]
---@field column integer
---@field line integer
---@field keywords table<string, boolean>
local Lexer = {}
Lexer.__index = Lexer

---@return Lexer
function Lexer.new()
	return setmetatable({ keywords = Keywords }, Lexer)
end

---@class Token
---@field start_column integer
---@field end_column integer
---@field start_line integer
---@field end_line integer
---@field kind TokenKind
---@field data any
local Token = {}
Token.__index = Token

---@param kind TokenKind
---@param data any?
---@return Token
function Token.new(kind, data)
	return setmetatable({ kind = kind, data = data }, Token)
end

---@return string
function Token:debug()
	return string.format("Token { start_column: %u, end_column: %u, start_line: %u, end_line: %u, data: %s }", self.start_column, self.end_column, self.start_line, self.end_line, self.data)
end

---@param source string
function Lexer:lex(source)
	local tokens = {}

	self.source = source
	self.column = 1
	self.line = 1
	self.ptr = 1

	while self.ptr < #self.source do
		local start_column, start_line = self.column, self.line

		local token = self:next()
		if not token then break end

		token.start_column = start_column
		token.start_line = start_line
		token.end_column = self.column
		token.end_line = self.line

		tokens[#tokens + 1] = token
	end

	return tokens
end

---@return Token
function Lexer:next()
	-- Ordered by which token is most likely to appear.
	local ws = self:consumePattern("^(%s+)", true)
	if ws then
		return Token.new(TokenKind.Whitespace)
	end

	local bin = self:consumePattern("^0b([01]+)")
	if bin then
		return Token.new(TokenKind.Binary, tonumber(bin, 2))
	end

	local hex = self:consumePattern("^(0x[%xp-+]+)")
	if hex then
		return Token.new(TokenKind.Hexadecimal, assert(tonumber(hex), "Invalid hex literal"))
	end

	local exp_num = self:consumePattern("^(%d+%.?%d*e[-+]?%d+)")
	if exp_num then
		return Token.new(TokenKind.Decimal, assert(tonumber(exp_num), "Invalid decimal literal"))
	end

	local decimal = self:consumePattern("^(%d+%.%d+)")
	if decimal then
		return Token.new(TokenKind.Decimal, tonumber(decimal))
	end

	local int = self:consumePattern("^(%d+)")
	if int then
		return Token.new(TokenKind.Hexadecimal, tonumber(int))
	end

	local ident = self:consumePattern("^([%w_]+)")
	if ident then
		if ident == "true" or ident == "false" then
			return Token.new(TokenKind.Boolean, ident == "true")
		elseif self.keywords[ident] then
			return Token.new(TokenKind.Keyword, ident)
		end

		return Token.new(TokenKind.Ident, ident)
	end

	local label = self:consumePattern("^::([%w_]+)::")
	if label then
		return Token.new(TokenKind.Label, label)
	end

	local in_comment = self:consumeString("--")

	local depth1 = self:consumePattern("^%[(=*)%[")
	if depth1 then
		local buf, nbuf = {}, 0

		while true do
			local content, depth2 = self:consumePattern("(.-)%](=*)%]")

			if not content then
				error("Unbalanced multiline " .. in_comment and "comment" or "string")
			end

			if depth2 == depth1 then
				nbuf = nbuf + 1
				buf[nbuf] = content

				-- End of comment/string
				if in_comment then
					return Token.new(TokenKind.Comment, { multiline = true, content = table.concat(buf, '', 1, nbuf) })
				else
					return Token.new(TokenKind.String, { multiline = true, content = table.concat(buf, '', 1, nbuf) })
				end
			else
				-- Nested comment
				nbuf = nbuf + 1
				buf[nbuf] = content .. "]" .. depth2 .. "]"
			end
		end
	elseif in_comment then
		local content = self:consumePattern("^([^\n]+)")
		return Token.new(TokenKind.Comment, { multiline = false, content = content })
	end

	local quote = ""
	if self:consumeChar("'") then
		quote = "'"
	elseif self:consumeChar("\"") then
		quote = "\""
	end

	if quote ~= "" then
		local buf, nbuf = {}, 0

		-- Overcomplicated string tokenizaton using string.find without patterns
		while true do
			local quote_pos, slash_pos = self.source:find(quote, self.ptr, true), self.source:find('\\', self.ptr, true)

			if slash_pos and slash_pos < quote_pos then
				local before = self.source:sub(self.ptr, slash_pos - 1)

				nbuf = nbuf + 1
				buf[nbuf] = before

				self.ptr = slash_pos + 1
				self.column = self.column + (slash_pos - self.ptr + 1) + 1

				local escape = self:at()
				if escape == "\n" then
					-- Escape single-line string to multiline.
					self.line = self.line + 1
					self.column = 1
					self.ptr = self.ptr + 1

					nbuf = nbuf + 1
					buf[nbuf] = "\n"
				elseif escape == "x" then
					self:consume()
					local hex = assert(self:consumePattern("^(%x%x)"), "Invalid escape")

					nbuf = nbuf + 1
					buf[nbuf] = string.char(tonumber(hex, 16))
				elseif ValidEscapes[escape] then
					self:consume()

					nbuf = nbuf + 1
					buf[nbuf] = ValidEscapes[escape]
				else
					error("Invalid escape: \\" .. escape)
				end
			elseif quote_pos then
				local before = self.source:sub(self.ptr, quote_pos - 1)

				local nl = before:find("\n", 1, true)
				if nl then
					error("Unfinished string near '" .. quote .. "'")
				end

				nbuf = nbuf + 1
				buf[nbuf] = before

				self.ptr = quote_pos + 1
				self.column = self.column + (quote_pos - self.ptr + 1) + 1

				break
			else
				error("Expected " .. quote .. " to end string")
			end
		end

		return Token.new(TokenKind.String, table.concat(buf, '', 1, nbuf))
	end

	if self:consumeString("...") then
		return Token.new(TokenKind.Grammar, GrammarKind.Vararg)
	elseif Grammar[self:at()] then
		local enum = Grammar[self:at()]
		self:consume()

		return Token.new(TokenKind.Grammar, enum)
	end

	error("Failed to consume a token. " .. self.source:sub(self.ptr))
end

---@param pattern string # Pattern to attempt to consume
---@param multiline boolean? # Whether such pattern can contain newlines. Default false.
---@return string?
---@return string?
---@return string?
function Lexer:consumePattern(pattern, multiline)
	local start_ptr, end_ptr, match, g1, g2 = self.source:find(pattern, self.ptr)
	if not start_ptr then return end

	if multiline then
		local _, newlines = match:gsub("\n", "")
		if newlines ~= 0 then
			local final_nl, final_char = match:find("\n[^\n]*$")

			self.line = self.line + newlines
			self.column = final_char - final_nl + 1
		else
			-- Line didn't change
			self.column = self.column + (end_ptr - start_ptr + 1)
		end
	else
		self.column = self.column + (end_ptr - start_ptr + 1)
	end

	self.ptr = end_ptr + 1
	return match, g1, g2
end

---@param match string
---@return boolean
function Lexer:consumeString(match)
	local len = #match
	if self.source:sub(self.ptr, self.ptr + len - 1) == match then
		self.ptr = self.ptr + len
		return true
	end

	return false
end

---@param char string
---@return boolean
function Lexer:consumeChar(char)
	if self.source:sub(self.ptr, self.ptr) == char then
		self.ptr = self.ptr + 1
		return true
	end

	return false
end

--- Consumes the next character.
--- # Safety
--- This function ignores whether this will go off the source.
---@param ws boolean? # Whether the character to consume can be whitespace. Default false
---@return boolean
function Lexer:consume(ws)
	if ws and self:at() == "\n" then
		self.line = self.line + 1
		self.column = 1
	else
		self.column = self.column + 1
	end

	self.ptr = self.ptr + 1
	return true
end

---@return string
function Lexer:at()
	return self.source:sub(self.ptr, self.ptr)
end

local code = [=========[
hello
xtte2_
_

5
5.5
5.73e5

0b1101
0xDEADBEEF

"          \
  \bahh! \
"

local
--[===[ --[=[  [[hello world]] ]=] ]===]

[[]]

::ba_aa::

""

.,;:(){}[]...

"str\x23str"

"\x22"
--+-><<====<=>=*%
]=========]

local l = Lexer.new()
local tbl = l:lex(code)

for k, v in pairs(tbl) do
	print(k, v:debug(), type(v.data) == "table" and v.data.content)
end