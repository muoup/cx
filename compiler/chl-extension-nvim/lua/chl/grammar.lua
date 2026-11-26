local paths = require("chl.paths")

local M = {}

local PARSER = vim.fs.joinpath(paths.plugin, "parser", "chl.so")
local HIGHLIGHTS = vim.fs.joinpath(paths.plugin, "parser", "highlights.scm")
local SETUP = vim.fs.joinpath(paths.checkout, "setup-extensions.sh")

local loaded = false
local warned = false

local function notify(message, level)
    vim.notify(message, level or vim.log.levels.INFO, { title = "chl" })
end

local function register()
    if not loaded then
        if not vim.uv.fs_stat(PARSER) or not vim.uv.fs_stat(HIGHLIGHTS) then
            return false
        end
        vim.treesitter.language.add("chl", { path = PARSER })
        vim.treesitter.query.set("chl", "highlights", table.concat(vim.fn.readfile(HIGHLIGHTS), "\n"))
        loaded = true
    end
    return true
end

local function start(buf)
    if register() then
        vim.treesitter.start(buf, "chl")
    elseif not warned then
        warned = true
        notify("Chloro parser is not built; run :CHLGrammarBuild", vim.log.levels.WARN)
    end
end

local function on_build(result)
    if result.code ~= 0 then
        notify("failed to build Chloro parser:\n" .. result.stderr .. result.stdout, vim.log.levels.ERROR)
        return
    end
    if loaded then
        notify("Chloro parser rebuilt; restart Neovim to load it")
        return
    end
    notify("Chloro parser built")
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.bo[buf].filetype == "chl" then
            start(buf)
        end
    end
end

function M.build()
    notify("building Chloro parser...")
    vim.system({ SETUP, "--grammar-only" }, { text = true }, vim.schedule_wrap(on_build))
end

function M.setup()
    vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("chl-grammar", { clear = true }),
        pattern = "chl",
        callback = function(args)
            start(args.buf)
        end,
    })
end

return M
