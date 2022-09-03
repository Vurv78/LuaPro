const ta_editor = document.getElementById("editor");
const ta_output = document.getElementById("output");
const s_mode = document.getElementById("mode");

const lua = fengari.lua;
const L = fengari.L;
const to_luastring = fengari.to_luastring;
const to_jsstring = fengari.to_jsstring;

lua.lua_atnativeerror(L, function (l) {
	lua.lua_pushstring(l, to_luastring('' + lua.lua_touserdata(l, -1)));
});

function update() {
	const code = ta_editor.value;
	const mode = s_mode.value;

	lua.lua_getglobal(L, to_luastring("Transpile"));
	lua.lua_pushstring(L, to_luastring(code));
	lua.lua_pushstring(L, to_luastring(mode));

	const res = lua.lua_pcall(L, 2, 1, 0);
	if (res != lua.LUA_OK) {
		const msg = "Error when running: " + fengari.to_jsstring(lua.lua_tostring(L, -1));

		console.log(msg);
		ta_output.value = msg;

		lua.lua_pop(L, 1);
	} else {
		const out_code = lua.lua_tostring(L, -1);
		ta_output.value = to_jsstring(out_code);
		lua.lua_pop(L, 1);
	}
}

s_mode.onchange = update;
ta_editor.onkeyup = update;

update();