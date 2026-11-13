local source = vim.fs.normalize(debug.getinfo(1, "S").source:sub(2))
local plugin = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(source)))
local compiler = vim.fs.dirname(plugin)

return {
    plugin = plugin,
    checkout = vim.fs.dirname(compiler),
    grammar = vim.fs.joinpath(compiler, "cx-treesitter-grammar"),
}
