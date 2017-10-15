;;; opencc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "opencc" "opencc.el" (23011 635 0 0))
;;; Generated autoloads from opencc.el

(autoload 'opencc-string "opencc" "\
按配置文件 CONFIG 转换字符串 STRING.

如果你没有自己的配置文件，请到在 `opencc-configuration-files' 中选择一个.

\(fn CONFIG STRING)" nil nil)

(autoload 'opencc-region "opencc" "\
按配置文件 CONFIG 转换 START 和 END 之间的文字.

如果你没有自己的配置文件，请到在 `opencc-configuration-files' 中选择一个.

\(fn CONFIG START END)" nil nil)

(autoload 'opencc-message "opencc" "\
一个交互命令，使用 minibuffer 和 echo area 读取输入和显示结果.

\(fn)" t nil)

(autoload 'opencc-replace-at-point "opencc" "\
一个交互命令，转化并替换光标下的文字.

\(fn)" t nil)

(autoload 'opencc-print-buffer "opencc" "\
一个交互命令，转化当前 Buffer 中的内容，在 *OpenCC Output* Buffer 中显示结果.

\(fn CONFIG &optional INPUT-BUFFER OUTPUT-BUFFER)" t nil)

(defvar opencc-insert-mode nil "\
Non-nil if Opencc-Insert mode is enabled.
See the `opencc-insert-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `opencc-insert-mode'.")

(custom-autoload 'opencc-insert-mode "opencc" nil)

(autoload 'opencc-insert-mode "opencc" "\
按照 `opencc-insert-mode-config' 转换并替换每一个输入的汉字.

\(fn &optional ARG)" t nil)

(defvar opencc-isearch-mode nil "\
Non-nil if Opencc-Isearch mode is enabled.
See the `opencc-isearch-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `opencc-isearch-mode'.")

(custom-autoload 'opencc-isearch-mode "opencc" nil)

(autoload 'opencc-isearch-mode "opencc" "\
输入简体搜索繁体（可通过 `opencc-isearch-mode-config' 调整转换的方向）.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; opencc-autoloads.el ends here
