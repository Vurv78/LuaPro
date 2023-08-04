local luapro = require("src.luapro")

local path_sep = package.config:sub(1, 1)
local traverse_cmd = path_sep == "\\" and "dir /b " or "ls "

---@param path string
---@param callback fun(path: string)
local function iterFiles(path, callback)
	path = string.gsub(path, "/", path_sep)

	local dir = io.popen(traverse_cmd .. path)
	for file in dir:lines() do
		callback(file)
	end
	dir:close()
end

local function deepEqual(t, t2)
	if #t ~= #t2 then return false end

	for k, v in pairs(t) do
		local v2 = t2[k]
		if type(v) == "table" then
			if type(v2) ~= "table" then return false end
			if not deepEqual(v, t2[k]) then return false end
		elseif v ~= v2 then
			return false
		end
	end

	for k, v in pairs(t2) do
		local v2 = t[k]
		if type(v) == "table" then
			if type(v2) ~= "table" then return false end
			if not deepEqual(v, v2) then return false end
		elseif v ~= v2 then
			return false
		end
	end
	return true
end

iterFiles("tests/tokenizer", function(path)
	local case = path:match("^(%d+)%.lua$")
	if not case then return end

	local case, expected =
		assert(io.open("tests/tokenizer/" .. path), "r", "io.open: " .. path),
		assert(io.open("tests/tokenizer/" .. case .. ".lua.expected"), "missing .expected file for " .. case)

	local tokens = luapro.tokenize(case:read("*a"))
	for i, tok in ipairs(tokens) do print(tok) end

	local ok, expected = pcall(loadstring, expected:read("*a"))
	assert(ok and expected, "Failed to compile expected result")
	local expected = assert(expected(), "no output from expected file " .. path)
	for i, tok in ipairs(expected) do print(tok) end
	assert(deepEqual(tokens, expected), "failed: " .. path)
end)

iterFiles("tests/parser", function(path)
	local case = path:match("^(%d+)%.lua$")
	if not case then return end

	local case, expected =
		assert(io.open("tests/parser/" .. path), "r", "io.open: " .. path),
		assert(io.open("tests/parser/" .. case .. ".lua.expected"), "missing .expected file for " .. case)

	local ast = luapro.parse( luapro.tokenize( case:read("*a") ) )
	local ok, expected = pcall(loadstring, expected:read("*a"))
	assert(ok and expected, "Failed to compile expected result")
	local expected = assert(expected(), "no output from expected file " .. path)

	print(luapro.format(ast))
	print(luapro.format(expected))
	assert(deepEqual(ast, expected), "failed: " .. path)
end)