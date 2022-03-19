## 🧐 Luaudit 🚧
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
* Formatting Lua 5.1 / LuaJIT code with indentation etc.
* Detects basic syntax errors
* Transforms hex escapes into their ASCII equivalents in strings for easy visibility.
* It's pure lua 🌔