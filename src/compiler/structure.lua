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

---@param op string
---@param precedence integer
local function Un(op, precedence)
	return { op, precedence, true }
end

---@param op string
---@param precedence integer
local function Bin(op, precedence)
	return { op, precedence, false }
end

local Operators = {
	-- In precedence order
	Bin("^", 1),

	Un("not", 2),
	Un("-", 2),
	Un("#", 2),

	Bin("*", 3),
	Bin("/", 3),
	Bin("%", 3),

	Bin("+", 4),
	Bin("-", 4),

	Bin("..", 5),

	Bin("==", 6),
	Bin("~=", 6),
	Bin(">", 6),
	Bin(">=", 6),
	Bin("<", 6),
	Bin("<=", 6),

	Bin("and", 7),
	Bin("or", 7),

	Bin("=", 8),
}

local OpChars = {}
for _, v in ipairs(Operators) do
	local op = v[1]
	for i = 1, #op do
		OpChars[ string.sub(op, i, i) ] = true
	end

	Operators[op] = v
end


local Grammar = LUT {
	":", ";", ",", "...", "(", ")", "[", "]", "{", "}", "."
}

return {
	Keywords = Keywords,
	Operators = Operators,
	OpChars = OpChars,
	Grammar = Grammar
}