#!/bin/bash
echo "Recompiling Emacs configuration and packages..."

EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"

# Recompile config files
echo "Recompiling config files..."
$EMACS --batch --eval "(byte-recompile-directory \"~/.emacs.d\" 0 t)"

# Refresh package contents and recompile all packages
echo "Recompiling packages..."
$EMACS --batch --eval "(progn (require 'package) (setq package-user-dir (expand-file-name \"elpa-31.0\" user-emacs-directory)) (package-initialize) (byte-recompile-directory package-user-dir 0 t))"

echo "Done! You can start Emacs normally now."

