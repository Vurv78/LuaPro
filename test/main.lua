package.path = package.path .. ";src/?.lua"

require("test/lib")

local path_sep = package.config:sub(1, 1)
local traverse_cmd = path_sep == "\\" and "dir /b " or "ls "

---@param path string
---@param callback fun(dir: string, file: string)
local function iterFiles(path, callback)
	path = string.gsub(path, "/", path_sep)

	local dir = io.popen(traverse_cmd .. path)
	for file in dir:lines() do
		local match = file:match("[%w%d_]+%.lua$")
		if match then
			callback(path, match)
		end
	end
	dir:close()
end

local failures = 0

---@param dir string
---@param file string
local function handleCase(dir, file)
	if file == "bench.lua" and os.getenv("NOBENCH") then return end

	local handle = assert( io.open(dir .. "/" .. file, "r"), "Failed to open file: " .. file)
	local code = handle:read("*a")

	local func = loadstring(code, "test case " .. file)
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
		Ansi.Print("{cyan}CASE {yellow}%s{white}: {green}OK", file)
	else
		failures = failures + 1
		Ansi.Print("{cyan}CASE {yellow}%s{white}: {red}FAILED {white}({magenta}" .. tostring(ret) .. "{white})", file)
		io.write( table.concat(out, "\t") )
	end
end

iterFiles("test/cases", handleCase)

if failures ~= 0 then
	Ansi.Print("{red}Failed {yellow}%u{red} tests.", failures)
	os.exit(1)
else
	Ansi.Print("{green}All tests passed.")
end