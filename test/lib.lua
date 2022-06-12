-- setfenv polyfill for Lua 5.2+
-- https://leafo.net/guides/setfenv-in-lua52-and-above.html#code
setfenv = setfenv or function(fn, env)
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

loadstring = loadstring or load

Assert = {}
function Assert.equal(a, b)
	if a ~= b then
		error(string.format("Assertion failed:\n\t%s\n\t~=\n\t%s", tostring(a), tostring(b)))
	end
end

bit = bit or bit32

if not bit then
	bit = {}

	-- Source: https://gist.github.com/kaeza/8ee7e921c98951b4686d
	-- Just need bit.band.

	local tconcat = table.concat
	local floor, max = math.floor, math.max
	local tonumber, assert, type = tonumber, assert, type

	local function tobittable_r(x, ...)
		if (x or 0) == 0 then return ... end
		return tobittable_r(floor(x / 2), x % 2, ...)
	end

	local function tobittable(x)
		assert(type(x) == "number", "argument must be a number")
		if x == 0 then return { 0 } end
		return { tobittable_r(x) }
	end

	local function makeop(cond)
		local function oper(x, y, ...)
			if not y then return x end
			x, y = tobittable(x), tobittable(y)
			local xl, yl = #x, #y
			local t, tl = { }, max(xl, yl)
			for i = 0, tl-1 do
				local b1, b2 = x[xl-i], y[yl-i]
				if not (b1 or b2) then break end
				t[tl-i] = (cond((b1 or 0) ~= 0, (b2 or 0) ~= 0)
				and 1 or 0)
			end
			return oper(tonumber(tconcat(t), 2), ...)
		end
		return oper
	end

	bit.band = makeop(function(a, b) return a and b end)
end