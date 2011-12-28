(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)
(setq clang-completion-flags
      '(
        "-pthread"
        "-g"
        "-I/usr/include"
        "-I/usr/lib/wx/include/gtk2-unicode-release-2.8"
        "-I/usr/include/wx-2.8"
        "-I/usr/include/gtk-2.0"
        "-I/usr/lib/gtk-2.0/include"
        "-I/usr/include/atk-1.0"
        "-I/usr/include/cairo"
        "-I/usr/include/gdk-pixbuf-2.0"
        "-I/usr/include/pango-1.0"
        "-I/usr/include/glib-2.0"
        "-I/usr/lib/glib-2.0/include"
        "-I/usr/include/pixman-1"
        "-I/usr/include/freetype2"
        "-I/usr/include/libpng14"
        "-Wno-write-strings"
        "-D_FILE_OFFSET_BITS=64"
        "-D_LARGE_FILES"
        "-D__WXGTK__"
        ))

(defun my-c-mode-common-hook()
  (setq ac-auto-start nil)
  (setq ac-expand-on-auto-complete nil)
  (setq ac-quick-help-delay 0.3)
  (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(provide 'init-auto-complete-clang)
