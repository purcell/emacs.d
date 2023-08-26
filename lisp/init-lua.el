;;; init-lua.el --- Support for Lua programming -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lua-mode)

(setq-default lua-indent-level 2)

(reformatter-define lua-format
  :program "lua-format"
  :args '("--indent-width=2" "--no-use-tab")
  :lighter "LuaFmt ")


(provide 'init-lua)
;;; init-lua.el ends here
