local source = vim.fs.normalize(debug.getinfo(1, "S").source:sub(2))
local plugin = vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(source)))

return {
    plugin = plugin,
    checkout = vim.fs.dirname(vim.fs.dirname(plugin)),
}
