local lib = require("src.lib.mod")

local tokenize = lib.tokenize
local Token = lib.Token

describe("keywords", function()
	it("should tokenize keywords as such", function()
		for _, kw in ipairs { "while", "do", "end", "if", "else", "then", "repeat", "goto", "elseif", "function", "until", "break", "for", "in" } do
			local tokens = tokenize(kw)

			expect(tokens).toBeDefined()
			expect(tokens).toHaveLength(1)

			local token = tokens[1]
			expect(token.variant).toBe(Token.Variant.Keyword)
			expect(token.data).toBe(kw)
		end
	end)
end)

describe("numbers", function()
	it("should tokenize a decimal number correctly", function()
		local tokens = tokenize([[1.2345]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.Decimal)
		expect(token.data).toBe(1.2345)
	end)

	it("should tokenize an integer correctly", function()
		local tokens = tokenize([[1]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.Integer)
		expect(token.data).toBe(1)

		local tokens = tokenize([[50]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.Integer)
		expect(token.data).toBe(50)
	end)

	it("should tokenize a hexadecimal number correctly", function()
		local tokens = tokenize([[0x5]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.Hexadecimal)
		expect(token.data).toBe(0x5)

		local tokens = tokenize([[0X123]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.Hexadecimal)
		expect(token.data).toBe(0X123)
	end)

	it("should tokenize a binary number correctly", function()
		local tokens = tokenize([[0b11101]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.Binary)
		expect(token.data).toBe(tonumber("11101", 2))
	end)
end)

describe("strings", function()
	it("should tokenize a string correctly", function()
		local tokens = tokenize([["f\no\bobar"]])

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.String)
		expect(token.data).toBe("f\\no\\bobar") -- probably not the expected output
	end)

	it("should tokenize a multiline string correctly", function()
		local tokens = tokenize([====[[==[foobar]==]]====]) -- [==[foobar]==]

		expect(tokens).toBeDefined()
		expect(tokens).toHaveLength(1)

		local token = tokens[1]
		expect(token.variant).toBe(Token.Variant.MString)
		expect(token.data).toBe("foobar")
	end)
end)