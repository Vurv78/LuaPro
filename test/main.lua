---@param path string
---@param callback fun(path: string)
local function iterFiles(path, callback)
	local dir = io.popen("ls " .. path)
	for file in dir:lines() do
		callback(file)
	end
	dir:close()
end

local failures = 0
iterFiles("test/cases", function(path)
	local handle = assert( io.open("test/cases/" .. path, "r"), "Failed to open file: " .. path)
	local code = handle:read("*a")

	local ok = pcall( loadstring(code, "test case " .. path) )
	if ok then
		print("CASE " .. path .. ": OK")
	else
		failures = failures + 1
		print("CASE " .. path .. ": FAILED")
	end
end)

if failures ~= 0 then
	print("Failed #" .. failures .. " tests. ")
	os.exit(1)
else
	print("All tests passed.")
end