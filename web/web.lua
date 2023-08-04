package.path = "../?.lua;" .. package.path

local luapro = require "src.luapro"

local function sort_values(a, b)
	if type(a) == "number" and type(b) == "number" then
		return a < b
	else
		return tostring(a) < tostring(b)
	end
end

local function inspect(object, depth, dumped)
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
			output[k + 1] = indent .. key .. " = " .. inspect(value, depth, dumped) .. ','
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

---@param code string
---@param mode "Tokens"|"AST"|"Formatter"
---@param version Version
function Transpile(code, mode, version)
	if mode == "Tokens" then
		return inspect(luapro.tokenize(code, version))
	elseif mode == "AST" then
		return inspect(luapro.parse(luapro.tokenize(code, version), version))
	elseif mode == "Formatter" then
		return luapro.format(luapro.parse(luapro.tokenize(code, version), version))
	else
		return "Unsupported Mode: " .. mode
	end
end