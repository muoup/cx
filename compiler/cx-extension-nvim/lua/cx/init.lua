local M = {}

function M.setup(opts)
    opts = opts or {}
    require("cx.lsp").setup(opts.cmd)
    require("cx.grammar").setup()
end

return M
