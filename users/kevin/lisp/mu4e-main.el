;;; mu4e-main.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2014 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smtpmail)      ;; the queing stuff (silence elint)
(require 'mu4e-utils)    ;; utility functions


(defconst mu4e~main-buffer-name " *mu4e-main*"
  "*internal* Name of the mu4e main view buffer.")

(defvar mu4e-main-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map "b" 'mu4e-headers-search-bookmark)
    (define-key map "B" 'mu4e-headers-search-bookmark-edit)

    (define-key map "s" 'mu4e-headers-search)
    (define-key map "q" 'mu4e-quit)
    (define-key map "j" 'mu4e~headers-jump-to-maildir)
    (define-key map "C" 'mu4e-compose-new)

    (define-key map "m" 'mu4e~main-toggle-mail-sending-mode)
    (define-key map "f" 'smtpmail-send-queued-mail)

    ;;
    (define-key map "U" 'mu4e-update-mail-and-index)
    (define-key map  (kbd "C-S-u")   'mu4e-update-mail-and-index)
    ;; for terminal users
    (define-key map  (kbd "C-c C-u") 'mu4e-update-mail-and-index)

    (define-key map "S" 'mu4e-interrupt-update-mail)
    (define-key map  (kbd "C-S-u") 'mu4e-update-mail-and-index)


    (define-key map "$" 'mu4e-show-log)
    (define-key map "A" 'mu4e-about)
    (define-key map "N" 'mu4e-news)
    (define-key map "H" 'mu4e-display-manual)
    map)

  "Keymap for the *mu4e-main* buffer.")
(fset 'mu4e-main-mode-map mu4e-main-mode-map)

(defvar mu4e-main-mode-abbrev-table nil)
(define-derived-mode mu4e-main-mode special-mode "mu4e:main"
  "Major mode for the mu4e main screen.
\\{mu4e-main-mode-map}."
  (use-local-map mu4e-main-mode-map)
  (setq truncate-lines t
        overwrite-mode 'overwrite-mode-binary)
  (set (make-local-variable 'revert-buffer-function) #'mu4e~main-view-real))


(defun mu4e~main-action-str (str &optional func-or-shortcut)
  "Highlight the first occurence of [.] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  (let ((newstr
         (replace-regexp-in-string
          "\\[\\(..?\\)\\]"
          (lambda(m)
            (format "[%s]"
                    (propertize (match-string 1 m) 'face 'mu4e-highlight-face)))
          str))
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lexical-let ((macro func-or-shortcut))
                      (lambda()(interactive)
                        (execute-kbd-macro macro)))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "\\[.+$" newstr)
                       (- (length newstr) 1) 'mouse-face 'highlight newstr) newstr))

;; NEW
;; This is the old `mu4e~main-view' function but without
;; buffer switching at the end.
(defun mu4e~main-view-real (ignore-auto noconfirm)
  (let ((buf (get-buffer-create mu4e~main-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert
       "* "
       (propertize "mu4e - mu for emacs version " 'face 'mu4e-title-face)
       (propertize  mu4e-mu-version 'face 'mu4e-header-key-face)

       ;; show some server properties; in this case; a big C when there's
       ;; crypto support, a big G when there's Guile support
       " "
       (propertize
        (concat
         (when (plist-get mu4e~server-props :crypto) "C")
         (when (plist-get mu4e~server-props :guile)  "G"))
        'face 'mu4e-title-face)
       "\n\n"
       (propertize "  Basics\n\n" 'face 'mu4e-title-face)
       (mu4e~main-action-str "\t* [j]ump to some maildir\n" 'mu4e-jump-to-maildir)
       (mu4e~main-action-str "\t* enter a [s]earch query\n" 'mu4e-search)
       (mu4e~main-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new)
       "\n"
       (propertize "  Bookmarks\n\n" 'face 'mu4e-title-face)
       ;; TODO: it's a bit uncool to hard-code the "b" shortcut...
       (mapconcat
        (lambda (bm)
          (let* ((query (nth 0 bm)) (title (nth 1 bm)) (key (nth 2 bm)))
            (mu4e~main-action-str
             (concat "\t* [b" (make-string 1 key) "] " title)
             (concat "b" (make-string 1 key)))))
        mu4e-bookmarks "\n")
       "\n\n"
       (propertize "  Misc\n\n" 'face 'mu4e-title-face)

       (mu4e~main-action-str "\t* [U]pdate email & database\n"
                             'mu4e-update-mail-and-index)

	;; show the queue functions if `smtpmail-queue-dir' is defined
       (if (file-directory-p smtpmail-queue-dir)
           (mu4e~main-view-queue)
         "")
	"\n"
	(mu4e~main-action-str "\t* [N]ews\n" 'mu4e-news)
	(mu4e~main-action-str "\t* [A]bout mu4e\n" 'mu4e-about)
	(mu4e~main-action-str "\t* [H]elp\n" 'mu4e-display-manual)
	(mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit))
      (mu4e-main-mode)
      )))

(defun mu4e~main-view-queue ()
  "Display queue-related actions in the main view."
  (concat
   (mu4e~main-action-str "\t* toggle [m]ail sending mode "
                         'mu4e~main-toggle-mail-sending-mode)
   "(currently "
   (propertize (if smtpmail-queue-mail "queued" "direct")
               'face 'mu4e-header-key-face)
   ")\n"
   (let ((queue-size (mu4e~main-queue-size)))
     (if (zerop queue-size)
         ""
       (mu4e~main-action-str
        (format "\t* [f]lush %s queued %s\n"
                (propertize (int-to-string queue-size)
                            'face 'mu4e-header-key-face)
                (if (> queue-size 1) "mails" "mail"))
        'smtpmail-send-queued-mail)))))

(defun mu4e~main-queue-size ()
  "Return, as an int, the number of emails in the queue."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents (expand-file-name smtpmail-queue-index-file
                                                smtpmail-queue-dir))
        (count-lines (point-min) (point-max)))
    (error 0)))

(defun mu4e~main-view ()
  "Create the mu4e main-view, and switch to it."
  (mu4e~main-view-real nil nil)
  (switch-to-buffer mu4e~main-buffer-name)
  (goto-char (point-min)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;; NEW
;; Toggle mail sending mode without switching
(defun mu4e~main-toggle-mail-sending-mode ()
  "Toggle sending mail mode, either queued or direct."
  (interactive)
  (let ((curpos (point)))
    (unless (file-directory-p smtpmail-queue-dir)
      (mu4e-error "`smtpmail-queue-dir' does not exist"))
    (setq smtpmail-queue-mail (not smtpmail-queue-mail))
    (message
     (concat "Outgoing mail will now be "
             (if smtpmail-queue-mail "queued" "sent directly")))
    (mu4e~main-view-real nil nil)
    (goto-char curpos)))


;; (progn
;;   (define-key mu4e-compose-mode-map (kbd "C-c m") 'mu4e~main-toggle-mail-sending-mode)
;;   (define-key mu4e-view-mode-map (kbd "C-c m")    'mu4e~main-toggle-mail-sending-mode)
;;   (define-key mu4e-compose-mode-map (kbd "C-c m") 'mu4e~main-toggle-mail-sending-mode)
;;   (define-key mu4e-headers-mode-map (kbd "C-c m") 'mu4e~main-toggle-mail-sending-mode)
;; )

(provide 'mu4e-main)
