package.path = package.path .. ";src/?.lua"

local Lexer = require("lexer/lua").new()
local Parser = require("parser/lua").new()

local Transpiler = require("codegen/lua")
local Formatter = Transpiler.new( require("codegen/mode-lua/format") )
local Deobfuscator = Transpiler.new( require("codegen/mode-lua/deobfuscate") )
local Optimizer = Transpiler.new( require("codegen/mode-lua/optimize") )

--- Indented print.
-- Removes indentation from the given string so you can indent the string source without it affecting the output.
---@param indent integer
local function printi(indent, msg)
	local unindented = string.gsub('\n' .. msg, '\n' .. string.rep('\t', indent), '\n' )
	print( unindented:sub(2) )
end

local Inspect

local Commands = {
	["help"] = function()
		printi(3, [[
			LuaPro [..]
			Hopeful lua deobfuscator, formatter, parser, etc

			USAGE:
				luapro <SUBCOMMAND>

			SUBCOMMANDS:
				format				Formats a script to the output file.
				deobfuscate			Deobfuscates a script. (Extends `format`)
				optimize			Optimizes a script. (Extends `format`)
				ast	 				Generates an AST for a script to the output file.
				lex	 				Lexes a script to the output file.
				version 			Prints version.
				help 				Prints this help.
		]])
	end,

	["version"] = function()
		print "LuaPro v0.1.0"
	end,

	["format"] = function()
		local input = assert(arg[2], "No input file specified.")
		local output = assert(arg[3], "No output file specified.")

		local file = assert( io.open(input, "rb"), "Could not open input file." )
		local toks = Lexer:parse(file:read "*a")
		local nodes = Parser:parse(toks)
		local code = Formatter:process(nodes)
		file:close()

		local out = assert( io.open(output, "wb"), "Could not open output file." )
		out:write(code)

		print("Formatted " .. input .. " to " .. output)
	end,

	["deobfuscate"] = function()
		local input = assert(arg[2], "No input file specified.")
		local output = assert(arg[3], "No output file specified.")

		local file = assert( io.open(input, "rb"), "Could not open input file." )
		local toks = Lexer:parse(file:read "*a")
		local nodes = Parser:parse(toks)
		local code = Deobfuscator:process(nodes)
		file:close()

		local out = assert( io.open(output, "wb"), "Could not open output file." )
		out:write(code)

		print("Deobfuscated " .. input .. " to " .. output)
	end,

	["optimize"] = function()
		local input = assert(arg[2], "No input file specified.")
		local output = assert(arg[3], "No output file specified.")

		local file = assert( io.open(input, "rb"), "Could not open input file." )
		local toks = Lexer:parse(file:read "*a")
		local nodes = Parser:parse(toks)
		local code = Optimizer:process(nodes)
		file:close()

		local out = assert( io.open(output, "wb"), "Could not open output file." )
		out:write(code)

		print("Optimized " .. input .. " to " .. output)
	end,

	["ast"] = function()
		local input = assert(arg[2], "No input file specified.")
		local output = assert(arg[3], "No output file specified.")

		local file = assert( io.open(input, "rb"), "Could not open input file." )
		local toks = Lexer:parse(file:read "*a")
		local nodes = Parser:parse(toks)

		file:close()

		do
			local handle = assert( io.open(output, "wb"), "Could not open output file." )
			handle:write( "local _ = " .. Inspect(nodes) )
			handle:close()

			print("Generated AST for " .. input .. " to " .. output)
		end
	end,

	["lex"] = function()
		local input = assert(arg[2], "No input file specified.")
		local output = assert(arg[3], "No output file specified.")

		local file = assert( io.open(input, "rb"), "Could not open input file." )
		local toks = Lexer:parse(file:read "*a")
		file:close()

		do
			local handle = assert( io.open(output, "wb"), "Could not open output file." )
			handle:write( "local _ = " .. Inspect(toks) )
			handle:close()
		end
	end
}

local function sort_values(a, b)
	if type(a) == "number" and type(b) == "number" then
		return a < b
	else
		return tostring(a) < tostring(b)
	end
end

-- Taken from my other project https://github.com/Vurv78/Expressive
function Inspect(object, depth, dumped)
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

if Commands[ arg[1] ] then
	Commands[ arg[1] ]()
else
	Commands["help"]()
end