local Lexer = require("compiler/lexer")
local Parser = require("compiler/parser")
local Transpiler = require("compiler/transpiler")

local handle = io.open("../in.lua", "rb")
local src = handle:read("*a")

local lexer = Lexer.new()
local tokens = lexer:parse(src)

for k, v in ipairs(tokens) do
	print("Token", k, v.val)
end

local parser = Parser.new()
local nodes = parser:parse(tokens)

for k, v in ipairs(nodes) do
	print("Node", k, v)
end

local transpiler = Transpiler.new()
local code = transpiler:process(nodes)

local handle = io.open("../out.lua", "wb")
handle:write(code)
handle:close()

local function sort_values(a, b)
	if type(a) == "number" and type(b) == "number" then
		return a < b
	else
		return tostring(a) < tostring(b)
	end
end

-- Taken from my other project https://github.com/Vurv78/Expressive
local function Inspect(object, depth, dumped)
	depth = depth or 0

	if dumped then
		local ref_depth = dumped[object]
		if ref_depth then
			return "<self " .. ref_depth .. ">"
		end
	else
		dumped = {}
	end

	local obj_type = type(object)

	if obj_type == "table" then
		local keys = {}

		do
			local idx = 1
			for key, _ in pairs(object) do
				keys[idx] = key
				idx = idx + 1
			end
		end

		table.sort(keys, sort_values)

		depth = depth + 1

		local output = {'{'}
		local indent = string.rep(' ', depth * 4)

		dumped[object] = depth
		for k, key in pairs(keys) do
			local ty, value = type(key), object[key]
			if ty == "number" then
				key = '[' .. key .. ']'
			elseif ty ~= "string" then
				key = '[' .. tostring(key) .. ']'
			end
			output[k + 1] = indent .. key .. " = " .. Inspect(value, depth, dumped) .. ','
		end
		dumped[object] = nil

		depth = depth - 1

		-- string.sub is faster than doing string.rep again. Remove the last 4 chars (indent)
		output[#output + 1] = string.sub(indent, 1, -4) .. '}'

		return table.concat(output, '\n')
	elseif obj_type == "string" then
		return string.format("%q", object)
	else
		return tostring(object)
	end
end

local handle = io.open("../ast.txt.lua", "wb")
handle:write( "local _ = " )
handle:write( Inspect(nodes) )
handle:close()