vim.api.nvim_create_user_command("CXGrammarBuild", function()
    require("cx.grammar").build()
end, { desc = "Generate and compile the CX tree-sitter parser" })
