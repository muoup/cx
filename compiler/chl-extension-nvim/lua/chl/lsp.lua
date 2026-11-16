local paths = require("chl.paths")

local M = {}

local BINARY = "chl-lsp"

local function command(cmd)
    if cmd then
        return cmd
    end
    local found = vim.fn.exepath(BINARY)
    if found ~= "" then
        return { found }
    end
    return { vim.fs.joinpath(paths.checkout, "target", "debug", BINARY) }
end

function M.setup(cmd)
    vim.lsp.config("chl", {
        cmd = command(cmd),
        filetypes = { "chl" },
        root_markers = { "chl.lua", ".git" },
    })
    vim.lsp.enable("chl")
end

return M
