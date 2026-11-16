local M = {}

function M.setup(opts)
    opts = opts or {}
    require("chl.lsp").setup(opts.cmd)
    require("chl.grammar").setup()
end

return M
