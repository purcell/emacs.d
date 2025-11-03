#!/bin/bash
echo "Recompiling Emacs configuration and packages..."

EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"

# Recompile config files
echo "Recompiling config files..."
$EMACS --batch --eval "(byte-recompile-directory \"~/.emacs.d\" 0 t)"

# Recompile all packages
echo "Recompiling packages..."
$EMACS --batch --eval "(progn (require 'package) (package-initialize) (package-reinitialize) (byte-recompile-directory package-user-dir 0 t))"

echo "Done! You can start Emacs normally now."

