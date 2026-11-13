local paths = require("cx.paths")

local M = {}

local BINARY = "cx-lsp"

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
    vim.lsp.config("cx", {
        cmd = command(cmd),
        filetypes = { "cx" },
        root_markers = { "cx.lua", ".git" },
    })
    vim.lsp.enable("cx")
end

return M
