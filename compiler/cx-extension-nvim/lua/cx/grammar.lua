local paths = require("cx.paths")

local M = {}

local PARSER = vim.fs.joinpath(paths.plugin, "parser", "cx.so")
local STAGING = vim.fs.joinpath(paths.plugin, "parser", "cx.build.so")
local HIGHLIGHTS = vim.fs.joinpath(paths.grammar, "queries", "highlights.scm")

local loaded = false
local warned = false

local function notify(message, level)
    vim.notify(message, level or vim.log.levels.INFO, { title = "cx" })
end

local function register()
    if not loaded then
        if not vim.uv.fs_stat(PARSER) then
            return false
        end
        vim.treesitter.language.add("cx", { path = PARSER })
        loaded = true
    end
    vim.treesitter.query.set("cx", "highlights", table.concat(vim.fn.readfile(HIGHLIGHTS), "\n"))
    return true
end

local function start(buf)
    if register() then
        vim.treesitter.start(buf, "cx")
    elseif not warned then
        warned = true
        notify("CX parser is not built; run :CXGrammarBuild", vim.log.levels.WARN)
    end
end

local function run(args, next)
    vim.system(args, { cwd = paths.grammar, text = true }, vim.schedule_wrap(function(result)
        if result.code ~= 0 then
            notify(table.concat(args, " ") .. " failed:\n" .. result.stderr, vim.log.levels.ERROR)
            return
        end
        next()
    end))
end

local function install()
    local _, err = vim.uv.fs_rename(STAGING, PARSER)
    if err then
        notify("failed to install CX parser: " .. err, vim.log.levels.ERROR)
        return
    end
    if loaded then
        notify("CX parser rebuilt; restart Neovim to load it")
        return
    end
    notify("CX parser built")
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.bo[buf].filetype == "cx" then
            start(buf)
        end
    end
end

function M.build()
    vim.fn.mkdir(vim.fs.dirname(PARSER), "p")
    notify("building CX parser...")
    run({ "tree-sitter", "generate" }, function()
        run({ "tree-sitter", "build", "-o", STAGING }, install)
    end)
end

function M.setup()
    vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("cx-grammar", { clear = true }),
        pattern = "cx",
        callback = function(args)
            start(args.buf)
        end,
    })
end

return M
