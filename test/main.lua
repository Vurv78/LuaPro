package.path = package.path .. ";src/?.lua"

require("test/lib")

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

	local func = loadstring(code, "test case " .. path)
	local out, nout = {}, 0
	setfenv(func, setmetatable({
		print = function(...)
			local nargs = select("#", ...)

			for i = 1, nargs do
				out[i + nout] = tostring(select(i, ...))
			end
			out[nout + nargs + 1] = "\n"
			nout = nout + nargs + 1
		end
	}, {
		__index = _G
	}))

	local ok, ret = xpcall(func, debug.traceback)
	if ok then
		print("CASE " .. path .. ": OK")
	else
		failures = failures + 1
		print("CASE " .. path .. ": FAILED (" .. tostring(ret) .. ")")
		io.write( table.concat(out, "\t") )
	end
end)

if failures ~= 0 then
	print("Failed #" .. failures .. " tests. ")
	os.exit(1)
else
	print("All tests passed.")
end