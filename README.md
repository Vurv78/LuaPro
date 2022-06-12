# 🧐 LuaPro [![CI Badge](https://github.com/Vurv78/LuaPro/actions/workflows/test.yml/badge.svg)](https://github.com/Vurv78/LuaPro/actions) [![License](https://img.shields.io/github/license/Vurv78/LuaPro?color=red&include_prereleases)](https://github.com/Vurv78/LuaPro/blob/master/LICENSE) [![github/Vurv78](https://img.shields.io/discord/824727565948157963?label=Discord&logo=discord&logoColor=ffffff&labelColor=7289DA&color=2c2f33)](https://discord.gg/yXKMt2XUXm)
> Lua formatter, deobsfuscator, parser, etc

### Turns
```lua
print("abc")local a,b=55,21;do print("xyz")end;repeat break;repeat print("Hi")(function(a,b,c)a,b=b,c;local d=0x50;local e="\x70\x72\x69\x6e\x74\x28\x27\x65\x76\x69\x6c\x20\x6c\x75\x61\x20\x63\x6f\x64\x65\x27\x29"end)()until true;local f,g,h;for i=1,2,3 do _G["\x6c\x6f\x61\x64\x73\x74\x72\x69\x6e\x67"]=55 end until true
```

### Into
```lua
print("abc")
local a, b = 55, 21
do
	print("xyz")
end
repeat
	break
	repeat
		print("Hi")
		(function(a, b, c)
			a, b = b, c
			local d = 80
			local e = "print('evil lua code')"
		end)()
	until true
	local f, g, h
	for i = 1, 2, 3 do
		_G["loadstring"] = 55
	end
until true
```

## Features
* It's written in pure, optimized lua (best with luajit) 🌔
* Formatting Lua 5.1 / LuaJIT code with proper indentation, spacing.
* `Deobsfuscate` mode on top of formatting to reveal complex literals (and will later optimize away garbage code).
* Supports 5.1, 5.2, 5.3 and LuaJIT (5.4 not tested)*

\* Note you will need to provide your own bit library polyfills for lua 5.3 or 5.1. (5.2 and jit are fine)
