--- Creates lookup table from array table
---@param tab table
---@return table
local function LUT(tab)
	local lut = {}
	for i, v in ipairs(tab) do
		lut[v] = i
	end
	return lut
end

local Keywords = LUT {
	"break", "do", "else", "elseif", "end", "for", "function",
	"if", "in", "local", "repeat", "return", "then",
	"until", "while", "nil"
}

local Operators = LUT {
	"^","%", "/", "*", "-", "+", "=", "..",
	"==", ">", ">=", "<", "<=", "~=",

	"or", "and", "not"
}

--- Returns a lookup table with every character from a table's string keys.
---@param t table
---@return table
local function charsOfKeys(t)
	local lut = {}
	for k, v in pairs(t) do
		for i = 1, #k do
			lut[k:sub(i, i)] = true
		end
	end
	return lut
end

local OpChars = charsOfKeys(Operators)

local Grammar = LUT {
	":", ";", ",", "...", "(", ")", "[", "]", "{", "}", "."
}

return {
	Keywords = Keywords,
	Operators = Operators,
	OpChars = OpChars,
	Grammar = Grammar
}