(require 'ert)

(ert-deftest wgrep-ag-normal ()
  :tags '(wgrep-subtest)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "HOGE\nFOO\nBAZ\n")
    (wgrep-test--ag "FOO|HOGE" "test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    (wgrep-goto-first-found)
    ;; search hit line (hit by -C option)
    (should (re-search-forward "HOGE" nil t))
    ;; delete 1st line
    (wgrep-mark-deletion)
    (should (re-search-forward "FOO" nil t))
    ;; replace 2nd line
    (replace-match "FOO2")
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "FOO2\nBAZ\n" (wgrep-test--get-contents "test-data.txt")))
    (wgrep-test--cleanup-file "test-data.txt")))

