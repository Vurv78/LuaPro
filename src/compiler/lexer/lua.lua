local Structure = require("compiler/structure/lua")
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

---@class TokenKinds
local KINDS = {
	Whitespace = 1,
	Comment = 2,
	MComment = 3,
	Boolean = 4,
	Keyword = 5,
	Decimal = 6,
	Integer = 7,
	Hexadecimal = 8,
	Octal = 9,
	Binary = 10,
	String = 11,
	MString = 12,
	Operator = 13,
	Grammar = 14,
	Identifier = 15
}

local KINDS_INV = {}
for k, v in pairs(KINDS) do
	KINDS_INV[v] = k
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

function Token:__tostring()
	return string.format("Token [%s] %s (#%u)", KINDS_INV[self.kind], self.raw and '"' .. self.raw .. '"' or "", self.startline)
end

---@class NomFlags
local NomFlags = {
	None      = 0b0000,
	Newlines  = 0b0001, -- Can the pattern have newlines?
	Number    = 0b0010, -- Get a value from the pattern w/ tonumber
	Reserved1 = 0b0100, -- Reserved for future use
	Reserved2 = 0b1000, -- Reserved for future use

	All       = 0b1111
}

---@param flags NomFlags
---@param pattern string
---@param lookup table<string, boolean>?
---@return fun(self: Lexer): boolean
local function nom(flags, pattern, lookup)
	assert( type(flags) == "number" and flags <= NomFlags.All, "Invalid flag " .. tostring(flags) )
	assert(pattern:sub(1, 2) == "^(", "Pattern " .. pattern .. " must start with '^('")

	if bit.band(flags, NomFlags.Newlines) ~= 0 then
		-- Can't have a lookup table.
		assert(not lookup, "Can't have a lookup table with newlines (nom)")

		---@param self Lexer
		return function(self)
			local match = string.match(self.input, pattern, self.pos)

			if match then
				local _, nls = string.gsub(match, "\n", "")
				if nls ~= 0 then
					self.startline = self.endline
					self.endline = self.endline + nls

					local afternl = assert( string.match(match, "\n(.*)$"), "Wtf??" )
					self.startcol = self.endcol + 1 -- maybe ??
					self.endcol = #afternl
				else
					-- Endline remains the same.
					self.startline = self.endline

					self.startcol = self.endcol + 1
					self.endcol = self.endcol + #match
				end

				self.pos = self.pos + #match

				return { raw = match }
			end
		end
	elseif bit.band(flags, NomFlags.Number) ~= 0 then
		assert(not lookup, "Can't have a lookup table with a number (nom)")

		---@param self Lexer
		return function(self)
			local match = string.match(self.input, pattern, self.pos)
			if match then
				self.startcol = self.endcol + 1
				self.endcol = self.endcol + #match + 1

				self.pos = self.pos + #match + 1
				return { raw = match, value = tonumber(match), negative = match:sub(1, 1) == "-" }
			end
		end
	end

	if lookup then
		---@param self Lexer
		return function(self)
			local match = string.match(self.input, pattern, self.pos)
			print(self.input:sub(self.pos, self.pos + 5), pattern, match)

			if lookup[match] ~= nil then
				self.startcol = self.endcol + 1
				self.endcol = self.endcol + #match

				self.pos = self.pos + #match + 1
				return { raw = match, data = lookup[match] }
			end
		end
	else
		return function(self)
			local match = string.match(self.input, pattern, self.pos)

			if match then
				self.startcol = self.endcol + 1
				self.endcol = self.endcol + #match

				self.pos = self.pos + #match
				return { raw = match }
			end
		end
	end
end

local todo = function() end

---@type table<number, fun(self: Lexer): Token?>
local Matchers = {
	[KINDS.Whitespace]  = nom(NomFlags.Newlines, "^(%s+)"),
	[KINDS.Comment]     = todo, -- nom(NomFlags.None, "^(--[^\n]+)"),
	[KINDS.MComment]    = todo,
	[KINDS.Boolean]     = nom(NomFlags.None,  "^(%l+)", LUT { "true", "false" }),
	[KINDS.Keyword]     = nom(NomFlags.None, "^(%f[%w_][%w_]+%f[^%w_])", Keywords ),
	[KINDS.Decimal]     = nom(NomFlags.Number, "^([-+]?[0-9]+%.[0-9]+)"),
	[KINDS.Integer]     = nom(NomFlags.Number, "^([-+]?[0-9]+)"),
	[KINDS.Hexadecimal] = nom(NomFlags.Number, "^([-+]?0x[%x]+)"),
	[KINDS.Octal]       = nom(NomFlags.Number, "^([-+]?0[%o]+)"),
	[KINDS.Binary]      = nom(NomFlags.Number, "^([-+]?0b[01]+)"), -- LuaJIT specific
	[KINDS.String]      = nom(NomFlags.Newlines, [[^(%b"")]]),
	[KINDS.MString]     = todo,
	[KINDS.Operator]    = nom(NomFlags.None, "^([-^n#*/%%+.=~<>ao][o=.r]?[td]?)", Operators), -- why am i doing this
	[KINDS.Grammar]     = nom(NomFlags.None, "^([.;,(){}%[%]]%.?%.?)", Grammar),
	[KINDS.Identifier]  = nom(NomFlags.None, "^([%a_][%w_]*)")
}

Lexer.Matchers = Matchers

--- Tokenizes a string into an array (sequential table) of tokens.
---@param input string # Expressive source code to tokenize
---@return table<number, Token>
function Lexer:parse(input)
	assert(type(input) == "string", "bad argument #1 to 'Lexer:parse' (string expected, got " .. type(input) .. ")")

	-- Reset so this tokenizer can be re-used
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
---@return boolean caught Whether the tokenizer caught something. This applies to everything, even stuff without data
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
				return true, setmetatable(token, Token)
			else
				return true
			end
		end
	end

	error( string.format("Could not parse token [%q] at line %u, cols %u-%u", input:sub(self.pos, self.pos + 5), self.startline, self.startcol, self.endcol) )
end

Lexer.Kinds = KINDS
return Lexer