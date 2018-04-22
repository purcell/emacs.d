(require 'ert)

(defun wgrep-test--wait (buf)
  (let ((proc (get-buffer-process buf)))
    (while (eq (process-status proc) 'run) 
      (sit-for 0.1))
    (sleep-for 0.2)
    (switch-to-buffer buf)))

(defun wgrep-test--grep (command)
  (let ((buf (grep command)))
    (wgrep-test--wait buf)))

(defun wgrep-test--ag (string file)
  (let ((buf (ag/search string default-directory :file-regex file :regexp t)))
    (wgrep-test--wait buf)))

(defun wgrep-test--get-contents (file &optional cs)
  (let ((coding-system-for-read cs))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun wgrep-test--prepare-file (file contents &optional cs)
  ;; cleanup for convinience
  (let ((buf (get-file-buffer file)))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (let ((coding-system-for-write cs))
    (write-region contents nil file)))

(defun wgrep-test--cleanup-file (file)
  (when (file-exists-p file)
    (delete-file file))
  (when (file-exists-p (concat file "~"))
    (delete-file (concat file "~"))))

(ert-deftest wgrep-normal ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "HOGE\nFOO\nBAZ\n")
    (wgrep-test--grep "grep -nH -e FOO -C 1 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; header is readonly
    (should (re-search-forward "^grep" nil t))
    (should-error (delete-char 1) :type 'text-read-only)
    ;; search hit line (hit by -C option)
    (should (re-search-forward "HOGE" nil t))
    ;; delete 1st line
    (wgrep-mark-deletion)
    (should (re-search-forward "FOO" nil t))
    ;; replace 2nd line
    (replace-match "FOO2")
    ;; footer is readonly
    (goto-char (point-max))
    (should-error (delete-char -1) :type 'text-read-only)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "FOO2\nBAZ\n" (wgrep-test--get-contents "test-data.txt")))
    (wgrep-test--cleanup-file "test-data.txt")))

(ert-deftest wgrep-normal-with-newline ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "HOGE\n")
    (wgrep-test--grep "grep -nH -e HOGE test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; through the header
    (should (re-search-forward "^test-data\\.txt:" nil t))
    ;; search hit line (hit by -C option)
    (should (re-search-forward "HOGE" nil t))
    (replace-match "FOO\nBAZ")
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "FOO\nBAZ\n" (wgrep-test--get-contents "test-data.txt")))
    (wgrep-test--cleanup-file "test-data.txt")))

(ert-deftest wgrep-bom-with-multibyte ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "あ\nい\nう\n" 'utf-8-with-signature)
    (wgrep-test--grep "grep -nH -e 'あ' -A 2 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; BOM check is valid.
    ;; skip BOM by `.*'
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(あ\\)$" nil t))
    (replace-match "へのへのも" nil nil nil 1)
    ;; 2nd line
    (should (re-search-forward "test-data\\.txt-[0-9]+-\\(い\\)$" nil t))
    (replace-match "へじ" nil nil nil 1)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "へのへのも\nへじ\nう\n" (wgrep-test--get-contents "test-data.txt")))
    (wgrep-test--cleanup-file "test-data.txt")))

(ert-deftest wgrep-bom-with-unibyte ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "a\nb\n" 'utf-8-with-signature)
    (wgrep-test--grep "grep -nH -e 'a' -A 2 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; BOM check is valid.
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(a\\)$" nil t))
    (replace-match "ABCD" nil nil nil 1)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "ABCD\nb\n" (wgrep-test--get-contents "test-data.txt")))
    (wgrep-test--cleanup-file "test-data.txt")))

(ert-deftest wgrep-with-modify ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "a\nb\nc\n")
    (with-current-buffer (find-file-noselect "test-data.txt")
      ;; modify file buffer
      (goto-char (point-min))
      (and (re-search-forward "^a" nil t)
           (replace-match "hoge"))
      (and (re-search-forward "^b" nil t)
           (replace-match "foo")))
    (wgrep-test--grep "grep -nH -e 'a' -A 2 test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    ;; delete "a" line (failed when saving)
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(a\\)$" nil t))
    (wgrep-mark-deletion)
    ;; replace "b" line (failed when saving)
    (should (re-search-forward "test-data\\.txt-[0-9]+-.*\\(b\\)$" nil t))
    (replace-match "B" nil nil nil 1)
    ;; replace "c" line
    (should (re-search-forward "test-data\\.txt-[0-9]+-.*\\(c\\)$" nil t))
    (replace-match "C" nil nil nil 1)
    ;; apply to buffer
    (wgrep-finish-edit)
    ;; save to file
    (wgrep-save-all-buffers)
    ;; compare file contents is valid
    (should (equal "hoge\nfoo\nC\n" (wgrep-test--get-contents "test-data.txt")))
    (wgrep-test--cleanup-file "test-data.txt")))

(ert-deftest wgrep-with-readonly-file ()
  :tags '(wgrep)
  (let (wgrep-auto-save-buffer)
    (wgrep-test--prepare-file "test-data.txt" "a\nb\nc\n")
    ;; make readonly
    (set-file-modes "test-data.txt" ?\400)
    (wgrep-test--grep "grep -nH -e 'a' test-data.txt")
    (wgrep-change-to-wgrep-mode)
    (goto-char (point-min))
    (should (re-search-forward "test-data\\.txt:[0-9]+:.*\\(a\\)$" nil t))
    (replace-match "A" nil nil nil 1)
    ;; only check with no error
    (wgrep-finish-edit))
  (wgrep-test--cleanup-file "test-data.txt"))

;; TODO (Not implemented testcase)
;; * wgrep-toggle-readonly-area
;; ** sort-lines
;; * wgrep-abort-changes
;; * wgrep-exit
;; * broken file contents (invalid coding system)
;; * new text contains newline
;; * wgrep-change-readonly-file
;; * test wgrep-*.el
