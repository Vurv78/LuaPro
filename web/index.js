import { html, render, useState } from "https://unpkg.com/htm/preact/standalone.module.js";

const DEFAULT_CODE = `while true do print("Hello, world!") end`;
const DEFAULT_MODE = "Formatter";
const DEFAULT_VERSION = "Lua 5.1";

function AllowTabs(e) {
	if (e.key == "Tab") {
		e.preventDefault();
		var start = e.target.selectionStart;
		var end = e.target.selectionEnd;

		// set textarea value to: text before caret + tab + text after caret
		e.target.value = e.target.value.substring(0, start) +
			"\t" + e.target.value.substring(end);

		// put caret at right position again
		e.target.selectionStart = e.target.selectionEnd = start + 1;
	}
}

function Playground() {
	let [code, setCode] = useState(DEFAULT_CODE);
	let [version, setVersion] = useState(DEFAULT_VERSION);
	let [output, setOutput] = useState("");
	let [mode, setMode] = useState(DEFAULT_MODE);

	fengari.lua.lua_getglobal(fengari.L, fengari.to_luastring("Transpile"));
	fengari.lua.lua_pushstring(fengari.L, fengari.to_luastring(code));
	fengari.lua.lua_pushstring(fengari.L, fengari.to_luastring(mode));
	fengari.lua.lua_pushstring(fengari.L, fengari.to_luastring(version));

	if (fengari.lua.lua_pcall(fengari.L, 3, 1, 0) != fengari.lua.LUA_OK) {
		setOutput("Error when running: " + fengari.to_jsstring(fengari.lua.lua_tostring(fengari.L, -1)));
	} else {
		setOutput(fengari.to_jsstring(fengari.lua.lua_tostring(fengari.L, -1)));
	}

	fengari.lua.lua_pop(fengari.L, 1);

	return html`
		<div class="header">
			<select name="mode" onchange=${e=> setMode(e.target.value)}>
				<option value="Formatter">Formatter</option>
				<option value="Tokens">Tokens</option>
				<option value="AST">AST</option>
			</select>

			<select name="version" onchange=${e=> setVersion(e.target.value)}>
				<option value="Lua 5.1">Lua 5.1</option>
				<option value="Lua 5.2">Lua 5.2</option>
				<option value="Lua 5.3">Lua 5.3</option>
				<option value="Lua 5.4">Lua 5.4</option>
				<option value="LuaJIT 2.1">LuaJIT 2.1</option>
				<option value="Garry's Mod">Garry's Mod</option>
			</select>
		</div>

		<div class="editors">
			<textarea
				id="editor"
				onkeydown=${AllowTabs}
				onkeyup=${e=> setCode(e.target.value)}
				spellcheck="false"
				aria-label="Lua code editor"
			>
				${DEFAULT_CODE}
			</textarea>

			<textarea
				id="output"
				spellcheck="false"
				aria-label="Output data, either formatted code, an AST, or a list of Tokens"
				readonly
			>
				${output}
			</textarea>
		</div>
	`;
}

fengari.lua.lua_atnativeerror(fengari.L, function (l) {
	fengari.lua.lua_pushstring(l, fengari.to_luastring('' + lua.lua_touserdata(l, -1)));
});

render(html`<${Playground} />`, document.body);