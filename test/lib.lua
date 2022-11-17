-- setfenv polyfill for Lua 5.2+
-- https://leafo.net/guides/setfenv-in-lua52-and-above.html#code
_G.setfenv = setfenv or function(fn, env)
	local i = 1
	while true do
		local name = debug.getupvalue(fn, i)
		if name == "_ENV" then
			debug.upvaluejoin(fn, i, function()
				return env
			end, 1)
			break
		elseif not name then
			break
		end

		i = i + 1
	end

	return fn
end

_G.loadstring = loadstring or load

_G.bit = bit or bit32

if not bit then
	_G.bit = {}

	function bit.band(a, b)
		local result = 0
		local bitval = 1
		while a > 0 and b > 0 do
			if a % 2 == 1 and b % 2 == 1 then -- test the rightmost bits
				result = result + bitval      -- set the current bit
			end
			bitval = bitval * 2 -- shift left
			a = math.floor(a/2) -- shift right
			b = math.floor(b/2)
		end
		return result
	end
end

Assert = {}
function Assert.equal(a, b)
	if a ~= b then
		if type(a) == "string" then
			error(string.format("Assert.equal failed:\n%s", Ansi.FmtDiff(a, b)))
		else
			error(string.format("Assert.equal failed:\n%s ~= %s", a, b))
		end
	end
end

local ansi = {
	reset      = "\27[0m",

	bright     = "\27[1m",
	dim        = "\27[2m",
	underline  = "\27[4m",
	blink      = "\27[5m",
	reverse    = "\27[7m",
	hidden     = "\27[8m",

	black     = "\27[30m",
	red       = "\27[31m",
	green     = "\27[32m",
	yellow    = "\27[33m",
	blue      = "\27[34m",
	magenta   = "\27[35m",
	cyan      = "\27[36m",
	white     = "\27[37m",

	blackbg   = "\27[40m",
	redbg     = "\27[41m",
	greenbg   = "\27[42m",
	yellowbg  = "\27[43m",
	bluebg    = "\27[44m",
	magentabg = "\27[45m",
	cyanbg    = "\27[46m",
	whitebg   = "\27[47m"
}

Ansi = {}

---@param fmt string
---@return string
function Ansi.Fmt(fmt, ...)
	local buf = {}
	local pos = 1

	while pos < #fmt do
		local next = string.find(fmt, "{", pos, true)
		if not next then
			buf[#buf + 1] = fmt:sub(pos)
			break
		end
		buf[#buf + 1] = fmt:sub(pos, next - 1)

		local closing = string.find(fmt, "}", next + 1, true)
		assert(closing, "Malformed printcolor argument")

		buf[#buf + 1] = "\27[0m"

		local inner = fmt:sub(next + 1, closing - 1)
		for word in inner:gmatch("%w+") do
			buf[#buf + 1] = assert(ansi[word], "Invalid color: " .. word)
		end

		pos = closing + 1
	end

	return string.format("\27" .. table.concat(buf) .. "\27[0m", ...)
end

---@param fmt string
function Ansi.Print(fmt, ...)
	print( Ansi.Fmt(fmt, ...) )
end

---@param s1 string
---@param s2 string
function Ansi.FmtDiff(s1, s2)
	local buf = {}

	local s1_len = #s1
	local s2_len = #s2

	for i = 1, math.min(s1_len, s2_len) do
		local c1, c2 = s1:sub(i, i), s2:sub(i, i)
		if c1 == c2 then
			buf[i] = c1
		else
			buf[i] = Ansi.Fmt("{green}%s", c1)
		end
	end

	if s1_len ~= s2_len then
		if s1_len > s2_len then
			-- s1 is greater (text lost)
			buf[#buf + 1] = Ansi.Fmt("{red}%s", s1:sub(s2_len + 1))
		else
			-- s2 is greater
			buf[#buf + 1] = Ansi.Fmt("{green}%s", s2:sub(s1_len + 1))
		end
	end

	return table.concat(buf)
end

---@param s1 string
---@param s2 string
function Ansi.PrintDiff(s1, s2)
	print( Ansi.FmtDiff(s1, s2) )
end

-- Ansi.PrintDiff("hello", "hello world")