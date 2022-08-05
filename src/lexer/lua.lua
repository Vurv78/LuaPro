--#region Prelude
local Structure = require("structure/lua")
local Operators, Keywords, Grammar, LUT = Structure.Operators, Structure.Keywords, Structure.Grammar, Structure.LUT

---@class Lexer
---@field input string
---@field pos number
---@field startcol number
---@field endcol number
---@field startline number
---@field endline number
local Lexer = {}
Lexer.__index = Lexer

function Lexer:reset()
	self.pos = 0
	self.startline = 1
	self.endline = 1

	self.startcol = 0
	self.endcol = 0
end

---@return Lexer
function Lexer.new()
	---@type Lexer
	local instance = setmetatable({}, Lexer)
	instance:reset()
	return instance
end

-- Hack to get proper token type with typed fields out of Parser:popToken() etc.
---@class TokenKinds
---@field Whitespace WhitespaceToken
---@field MComment MCommentToken
---@field Comment CommentToken
---@field Boolean BooleanToken
---@field Keyword KeywordToken
---@field Label LabelToken
---@field Hexadecimal HexadecimalToken
---@field Binary BinaryToken
---@field Decimal DecimalToken
---@field Integer IntegerToken
---@field String StringToken
---@field MString MStringToken
---@field Operator OperatorToken
---@field Grammar GrammarToken
---@field Identifier IdentifierToken
local KINDS = {}
local KINDS_INV = {}

local _kinds = {
	"Whitespace", "MComment", "Comment",
	"Boolean", "Keyword", "Label",
	"Hexadecimal", "Binary","Decimal", "Integer",
	"String",
	"MString", "Operator", "Grammar",
	"Identifier"
}

for k, v in ipairs(_kinds)  do
	KINDS[v] = k
	KINDS_INV[k] = v
end

---@class Token
---@field kind number
---@field value any? # Value of the token if it can immediately be determined. This would be for literals (numbers, strings, bools)
---@field data table # Misc lexed data from the tokens
---@field raw string? # Raw string of the token. Useful for grouped tokens like Grammar. Alias of data.raw
---@field startcol number
---@field endcol number
---@field startline number
---@field endline number
local Token = {}
Token.__index = Token

---@class LiteralToken: Token
---@field value number|string|boolean

---@class NumericToken: LiteralToken
---@field value number

---@class WhitespaceToken:    Token
---@class MCommentToken:      Token
---@class CommentToken:       Token
---@class BooleanToken:       LiteralToken
---@class KeywordToken:       Token
---@class LabelToken:         Token
---@class HexadecimalToken:   NumericToken
---@class BinaryToken:        NumericToken
---@class DecimalToken:       NumericToken
---@class IntegerToken:       NumericToken
---@class StringToken:        LiteralToken
---@class MStringToken:       LiteralToken
---@class OperatorToken:      Token
---@class GrammarToken:       Token
---@class IdentifierToken:    Token

function Token:__tostring()
	return string.format("Token [%s] %q L%u(%u-%u)", KINDS_INV[self.kind], self.raw, self.startline, self.startcol, self.endcol)
end

-- Avoid use of binary literals since they require luajit 2.1+ and regular lua doesn't support them at all.
local function b(n)
	return tonumber(n, 2)
end

---@class NomFlags
local NomFlags = {
	None      = b"0000",
	Newlines  = b"0001", -- Can the pattern have newlines?
	Value     = b"0010", -- Get a value from the pattern w/ simple functions like tonumber
	Delimited = b"0100", -- Delimited bracket stuff (Strings and Comments)
	Reserved  = b"1000", --

	All       = b"1111"
}

--#endregion

--- Get the stuff in between multiline strings and comments.
---@param str string
---@return string inner
---@return string depth
local function inner_braces(str)
	return str:match("%[=*%[([^%]]*)%](=*)%]$")
end

---@param flags NomFlags
---@param pattern string
---@param lookup table<string, boolean>?
---@param handle_val (fun(match: string): any)?
---@return fun(self: Lexer): boolean
local function nom(flags, pattern, lookup, handle_val)
	assert( type(flags) == "number" and flags <= NomFlags.All, "Invalid flag " .. tostring(flags) )

	---@param self Lexer
	local function resolve(self)
		return self.input:find(pattern, self.pos)
	end

	---@param match string
	---@return Token
	local function make(match)
		return { raw = match, data = {} }
	end

	-- Can be combined w/ Newlines flag for stuff like comments.
	-- Replaces the default pattern search with one based on lua's bracket delimiting for multiline strings and comments.
	if bit.band(flags, NomFlags.Delimited) ~= 0 then
		local prefix = pattern -- Could be prefixed by '--' for a comment.
		---@param self Lexer
		function resolve(self)
			local start, ed, depth = self.input:find("^" .. prefix .. "%[(=*)%[", self.pos)
			if start then
				local _, ed2 = self.input:find("]" .. ('='):rep(#depth) .. "]", ed + 1, true)
				if ed2 then
					return start, ed2, self.input:sub(start, ed2)
				end
			end
		end

		---@param match string
		---@return MCommentToken | MStringToken
		function make(match)
			local inner, depth = inner_braces(match)
			return { raw = match, value = inner, data = { #depth } }
		end
	else
		assert( pattern:sub(1, 2) == "^(", "Pattern " .. pattern .. " must start with '^('" )

		if bit.band(flags, NomFlags.Value) ~= 0 then
			handle_val = handle_val or lookup
			assert(handle_val ~= nil and type(handle_val) == "function", "Must provide a value handler w/ NomFlags.Value (nom)")

			function make(match)
				---@diagnostic disable-next-line (False positive shut up sumneko lua)
				return { raw = match, value = handle_val(match) }
			end
		end
	end

	if bit.band(flags, NomFlags.Newlines) ~= 0 then
		---@param self Lexer
		return function(self)
			local _, ed, match = resolve(self)

			if match then
				local _, nls = match:gsub("\n", "")
				if nls ~= 0 then
					self.startline = self.endline
					self.endline = self.endline + nls

					local afternl = assert( match:match("\n(.*)$"), "Wtf??" )
					self.startcol = self.endcol + 1 -- maybe ??
					self.endcol = #afternl
				else
					-- Endline remains the same.
					self.startline = self.endline

					self.startcol = self.endcol + 1
					self.endcol = self.endcol + #match
				end

				self.pos = ed + 1
				return make(match)
			end
		end
	end

	if lookup and type(lookup) == "table" then
		function make(match)
			return { raw = match, data = lookup[match] }
		end

		---@param self Lexer
		return function(self)
			local _, ed, match = resolve(self)

			if lookup[match] ~= nil then
				self.startcol = self.endcol + 1
				self.endcol = self.endcol + #match

				self.pos = ed + 1
				return make(match)
			end
		end
	else
		return function(self)
			local _, ed, match = resolve(self)

			if match then
				self.startcol = self.endcol + 1
				self.endcol = self.endcol + #match

				self.pos = ed + 1
				return make(match)
			end
		end
	end
end

--#region Matchers
local function val_bool(v)
	return v == "true"
end

local function val_comment(str)
	return str:sub(3)
end

local function val_label(str)
	return str:sub(3, -3)
end

local function val_string(s)
	return s:sub(2, -2)
end

local val_number = tonumber

local F = NomFlags
local Matchers = {
	[KINDS.Whitespace]  = nom(F.Newlines, "^(%s+)"),
	[KINDS.MComment]    = nom(F.Value + F.Delimited + F.Newlines, "%-%-", inner_braces),
	[KINDS.Comment]     = nom(F.Value, "^(%-%-[^\n]*)", val_comment),
	[KINDS.Boolean]     = nom(F.None,  "^(%l+)", LUT { "true", "false" }, val_bool),
	[KINDS.Keyword]     = nom(F.None, "^(%l+)", Keywords ),
	[KINDS.Label]       = nom(F.Value, "^(::[%w_]+::)", val_label),
	[KINDS.Hexadecimal] = nom(F.Value, "^(0x[%x]+)", val_number),
	[KINDS.Binary]      = nom(F.Value, "^(0b[01]+)", val_number), -- LuaJIT specific
	[KINDS.Decimal]     = nom(F.Value, "^([0-9]+%.[0-9]+)", val_number),
	[KINDS.Integer]     = nom(F.Value, "^([0-9]+)", val_number),
	[KINDS.String]      = nom(F.Value + F.Newlines, [[^(['"][^"']*['"])]], val_string),
	[KINDS.MString]     = nom(F.Value + F.Delimited + F.Newlines, "", inner_braces),
	[KINDS.Operator]    = nom(F.None, "^([-^n#*/%%+.=~<>ao][o=.r]?[td]?)", Operators), -- why am i doing this
	[KINDS.Grammar]     = nom(F.None, "^([.;,(){}%[%]]%.?%.?)", Grammar),
	[KINDS.Identifier]  = nom(F.None, "^([%a_][%w_]*)")
}

Lexer.Matchers = Matchers
--#endregion

--- Lexes a string into an array (sequential table) of tokens.
---@param input string # Source code to lex
---@return table<number, Token>
function Lexer:parse(input)
	assert(type(input) == "string", "bad argument #1 to 'Lexer:parse' (string expected, got " .. type(input) .. ")")

	-- Reset so it can be re-used
	self:reset()
	self.input = input

	local tokens, ntokens = {}, 0
	local caught, token = self:next()

	while caught do
		if token then
			ntokens = ntokens + 1
			tokens[ntokens] = token
		end
		caught, token = self:next()
	end

	return tokens
end

--- # Safety
--- This will throw an error with invalid syntax, so make sure to pcall.
---@return boolean caught Whether the lexer caught something. This applies to everything, even stuff without data
---@return Token? tok Table with token id and value
function Lexer:next()
	local input = self.input
	if self.pos > #input then return false end

	for kind, matcher in ipairs(Matchers) do
		local token = matcher(self)

		if token then
			if kind ~= KINDS.Whitespace then
				token.kind = kind
				token.startcol = self.startcol
				token.endcol = self.endcol
				token.startline = self.startline
				token.endline = self.endline
				self.startline = self.endline

				return true, setmetatable(token, Token)
			else
				self.startline = self.endline
				return true
			end
		end
	end

	error( string.format("Could not parse token [%q] at line %u, cols %u-%u", input:sub(self.pos, self.pos + 5), self.startline, self.startcol, self.endcol) )
end

Lexer.Kinds = KINDS
Lexer.KINDS_INV = KINDS_INV
return Lexer