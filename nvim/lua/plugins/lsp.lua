return {
    -- Main LSP Configuration
    -- Language servers come from the flake (Home Manager), not Mason.
    -- The enabled set is ~/.local/share/nvim/nix-lsp.lua (generated on rebuild).
    -- nvim-lspconfig only supplies server defaults; enable via vim.lsp.* (Nvim 0.11+).
    "neovim/nvim-lspconfig",
    dependencies = {
        -- Useful status updates for LSP.
        { "j-hui/fidget.nvim", opts = {} },

        -- Allows extra capabilities provided by nvim-cmp
        "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
        vim.api.nvim_create_autocmd("LspAttach", {
            group = vim.api.nvim_create_augroup("kickstart-lsp-attach", { clear = true }),
            callback = function(event)
                local map = function(keys, func, desc, mode)
                    mode = mode or "n"
                    vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
                end

                -- LSP Navigation/Action Keymaps
                map("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
                map("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
                map("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
                map("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")
                map("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
                map("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
                map("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
                map("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction", { "n", "x" })
                map("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")

                map("<leader>f", function()
                    vim.lsp.buf.format({ async = true })
                end, "Format document")

                local client = vim.lsp.get_client_by_id(event.data.client_id)

                if client and client:supports_method("textDocument/formatting", event.buf) then
                    vim.api.nvim_create_autocmd("BufWritePre", {
                        buffer = event.buf,
                        desc = "Format on save (LSP)",
                        callback = function()
                            vim.lsp.buf.format({ async = true })
                        end,
                    })
                end

                if client and client:supports_method("textDocument/documentHighlight", event.buf) then
                    local highlight_augroup = vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
                    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
                        buffer = event.buf,
                        group = highlight_augroup,
                        callback = vim.lsp.buf.document_highlight,
                    })

                    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
                        buffer = event.buf,
                        group = highlight_augroup,
                        callback = vim.lsp.buf.clear_references,
                    })

                    vim.api.nvim_create_autocmd("LspDetach", {
                        group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
                        callback = function(event2)
                            vim.lsp.buf.clear_references()
                            vim.api.nvim_clear_autocmds({ group = "kickstart-lsp-highlight", buffer = event2.buf })
                        end,
                    })
                end

                if client and client:supports_method("textDocument/inlayHint", event.buf) then
                    map("<leader>th", function()
                        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
                    end, "[T]oggle Inlay [H]ints")
                end
            end,
        })

        local capabilities = vim.lsp.protocol.make_client_capabilities()
        capabilities = vim.tbl_deep_extend("force", capabilities, require("cmp_nvim_lsp").default_capabilities())

        -- Default capabilities for all servers (Nix provides binaries on PATH)
        vim.lsp.config("*", { capabilities = capabilities })

        -- Extra settings only. Which servers to enable comes from the flake
        -- (~/.local/share/nvim/nix-lsp.lua, written by Home Manager).
        ---@type table<string, vim.lsp.Config>
        local extra = {
            html = { filetypes = { "html", "twig", "hbs" } },
            nixd = {
                settings = {
                    nixd = {
                        formatting = { command = { "nixfmt" } },
                    },
                },
            },
            lua_ls = {
                settings = {
                    Lua = {
                        completion = { callSnippet = "Replace" },
                        runtime = { version = "LuaJIT" },
                        workspace = {
                            checkThirdParty = false,
                            library = {
                                "${3rd}/luv/library",
                                unpack(vim.api.nvim_get_runtime_file("", true)),
                            },
                        },
                        diagnostics = { disable = { "missing-fields" } },
                        format = { enable = false },
                    },
                },
            },
        }

        -- Fallback if Home Manager has not generated the flake list yet.
        ---@type table<string, vim.lsp.Config>
        local fallback = {
            clangd = {},
            gopls = {},
            rust_analyzer = {},
            ts_ls = {},
            ruff = {},
            html = {},
            cssls = {},
            tailwindcss = {},
            dockerls = {},
            sqls = {},
            jsonls = {},
            yamlls = {},
            lua_ls = {},
            ocamllsp = {},
            nixd = {},
        }

        local function load_nix_servers()
            local path = vim.fn.stdpath("data") .. "/nix-lsp.lua"
            local ok, servers = pcall(dofile, path)
            if ok and type(servers) == "table" then
                return servers
            end
            return nil
        end

        local servers = load_nix_servers() or fallback

        for server_name, server in pairs(servers) do
            local cfg = vim.tbl_deep_extend("force", extra[server_name] or {}, server)
            vim.lsp.config(server_name, cfg)
            local merged = vim.lsp.config[server_name]
            -- Some servers (e.g. rust_analyzer) set cmd as a function, not a list.
            local cmd = merged and merged.cmd
            local bin = type(cmd) == "table" and cmd[1] or nil
            if type(bin) ~= "string" or vim.fn.executable(bin) == 1 then
                vim.lsp.enable(server_name)
            end
        end
    end,
}
