local Structure = require("compiler/structure")
local Operators, Keywords, LetterOperators, OpChars, Grammar = Structure.Operators, Structure.Keywords, Structure.LetterOperators, Structure.OpChars, Structure.Grammar

local SimpleEscapes = {
	["r"] = "\r",
	["n"] = "\n",
	["t"] = "\t",
	["b"] = "\b",
	["f"] = "\f",
	["a"] = "\a",
	["v"] = "\v",
	["\\"] = "\\",
	["\""] = "\"",
	["'"] = "'",
}

---@param char string
---@return boolean
local function is_number(char)
	return char ~= nil and char >= "0" and char <= "9"
end

---@param char string
---@return boolean
local function is_letter(char)
	return char ~= nil and (char >= "a" and char <= "z") or (char >= "A" and char <= "Z")
end

---@param char string
---@return boolean
local function is_hex(char)
	return char ~= nil and (char >= "a" and char <= "f") or (char >= "A" and char <= "F") or is_number(char)
end

---@param char string
---@return boolean
local function is_alphanumeric(char)
	return char ~= nil and ( (char >= "a" and char <= "z") or (char >= "A" and char <= "Z") or (char >= "0" and char <= "9") )
end

---@return boolean
local function is_whitespace(char)
	return char ~= nil and (char == " " or char == "\t" or char == "\n" or char == "\r")
end

---@param char string
---@return boolean
local function is_ident(char)
	return is_alphanumeric(char) or char == "_"
end

---@param char string
---@return boolean
local function is_opchar(char)
	return char ~= nil and Operators[char]
end

---@class Token
---@field kind integer
---@field raw string
---@field val any
local Token = {}
Token.__index = Token

function Token.new(kind, raw, val)
	return setmetatable({kind = kind, raw = raw, val = val}, Token)
end

---@class TokenKinds
local KINDS = {
	Comment = 1,
	MComment = 2,
	Boolean = 3,
	Keyword = 4,
	Number = 5, -- 0b0101 0x1010 55
	String = 6,
	MString = 7,
	Operator = 8,
	Grammar = 9,
	Ident = 10
}

local KINDS_INV = {}
for k, v in pairs(KINDS) do
	KINDS_INV[v] = k
end

function Token:__tostring()
	return KINDS_INV[self.kind] .. ": " .. self.raw
end

---@class Lexer
local Lexer = {}
Lexer.__index = Lexer

function Lexer.new()
	return setmetatable({}, Lexer)
end

---@param src string
---@return table<number, Token>
function Lexer:parse(src)
	-- Pass through and ignore whitespace (besides newlines)
	local tokens = {}

	local ptr, length = 0, #src
	local line, column = 0, 1

	local function nextChar()
		ptr = ptr + 1
		column = column + 1

		local s = string.sub(src, ptr, ptr)
		if s ~= "" then
			return s
		end
	end

	local function peekChar()
		local s = string.sub(src, ptr + 1, ptr + 1)
		if s ~= "" then
			return s
		end
	end

	local function prevChar()
		ptr = ptr - 1
		column = column - 1
		return string.sub(src, ptr, ptr)
	end

	local function nextLine()
		line = line + 1
		column = 1
	end

	while ptr < length do
		local char = nextChar()
		if char == "\n" then
			nextLine()
		elseif char == " " or char == "\t" or char == "\r" then
			-- Skip
		elseif char == "'" or char == "\"" then
			-- Match single line string WITH char escapes
			local str = {}
			local escaped = false
			local quot = char

			local str_start = ptr
			local raw
			while true do
				char = assert( nextChar(), "Trailing string" )

				if char == quot then
					if escaped then
						str[ #str + 1 ] = char
						escaped = false
					else
						raw = string.sub(src, str_start, ptr)
						break
					end
				elseif char == "\\" then
					if escaped then
						str[ #str + 1 ] = char
						escaped = false
					elseif SimpleEscapes[peekChar()] then
						str[ #str + 1 ] = SimpleEscapes[nextChar()]
					elseif peekChar() == "x" then
						local FAIL = "Invalid escape sequence"
						nextChar()

						local num = tonumber( assert( nextChar(), FAIL ) .. assert( nextChar(), FAIL ), 16 )
						str[ #str + 1 ] = string.char(num)
					else
						escaped = true
					end
				elseif char == "\n" then
					error("String using normal quotes cannot be multiline")
				else
					str[ #str + 1 ] = char
				end
			end

			tokens[#tokens + 1] = Token.new(KINDS.String, raw, table.concat(str))
		elseif is_number(char) then
			-- Match number (decimal)
			local num = { char }
			local previous_period, has_period = false, false

			local next = nextChar()
			if next == "b" then
				-- Binary 0b10101
				num[2] = next

				local char = nextChar()
				while char == "0" or char == "1" do
					num[#num + 1] = char
					char = nextChar()
				end
			elseif next == "x" then
				-- Hexadecimal
				num[2] = next

				local char = nextChar()
				while is_number(char) or (char >= "a" and char <= "f") or (char >= "A" and char <= "F") do
					num[#num + 1] = char
					char = nextChar()
				end
			elseif next == "e" then
				-- Exponent
				num[2] = next

				local char = peekChar()
				if char == "-" or char == "+" then
					num[#num + 1] = nextChar()
					char = nextChar()
				end

				while is_number(peekChar()) do
					num[#num + 1] = nextChar()
				end
			elseif is_number(next) then
				-- Decimal
				num[2] = next

				local char = nextChar()
				while char do
					if char == "." then
						assert(not has_period, "Multiple periods in number")
						assert(not previous_period, "Expected number after period")

						previous_period = true
						has_period = true
					elseif is_number(char) then
						if previous_period then
							previous_period = false
						end
					elseif previous_period then
						error("Expected number after period")
					else
						break
					end

					num[#num + 1] = char
					char = nextChar()
				end
			elseif not is_whitespace(char) then
				-- Error
				error("Expected number, got " .. char)
			end

			local raw = table.concat(num)
			tokens[#tokens + 1] = Token.new( KINDS.Number, raw, tonumber(raw) )
		elseif is_ident(char) then
			local ident = { char }
			while is_ident(peekChar()) do
				ident[#ident + 1] = nextChar()
			end

			local raw = table.concat(ident)
			if Keywords[raw] then
				tokens[#tokens + 1] = Token.new(KINDS.Keyword, raw, raw)
			elseif Operators[raw] then -- not, or, and
				tokens[#tokens + 1] = Token.new(KINDS.Operator, raw, raw)
			elseif raw == "true" then
				tokens[#tokens + 1] = Token.new(KINDS.Boolean, raw, true)
			elseif raw == "false" then
				tokens[#tokens + 1] = Token.new(KINDS.Boolean, raw, false)
			else
				tokens[#tokens + 1] = Token.new(KINDS.Ident, raw, raw)
			end
		elseif Grammar[char] then
			local grammar = char

			while peekChar() do
				if Grammar[grammar .. peekChar()] then
					grammar = grammar .. nextChar()
				else
					break
				end
			end

			tokens[#tokens + 1] = Token.new(KINDS.Grammar, grammar)
		elseif OpChars[char] then
			local op = char .. peekChar()
			if Operators[op] then
				nextChar()
				tokens[#tokens + 1] = Token.new(KINDS.Operator, op, op)
			elseif Operators[char] then
				tokens[#tokens + 1] = Token.new(KINDS.Operator, char, char)
			else
				error("Unknown operator " .. op)
			end
		else
			error("Unexpected end of input")
		end
	end
	return tokens
end

Lexer.Kinds = KINDS

return Lexer