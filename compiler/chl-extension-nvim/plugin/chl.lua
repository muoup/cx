vim.api.nvim_create_user_command("CHLGrammarBuild", function()
    require("chl.grammar").build()
end, { desc = "Generate and compile the Chloro tree-sitter parser" })
