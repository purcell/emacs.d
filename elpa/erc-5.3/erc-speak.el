;;; erc-speak.el --- Speech-enable the ERC chat client

;; Copyright 2001, 2002, 2003, 2004, 2007,
;;   2008 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains code to speech enable ERC using Emacspeak's functionality
;; to access a speech synthesizer.
;;
;; It tries to be intelligent and produce actually understandable
;; audio streams :). Hopefully it does. I use it on #debian at irc.debian.org
;; with about 200 users, and I am amazed how easy it works.
;;
;; Currently, erc-speak is only written to listen to channels.
;; There is no special functionality for interaction in the erc buffers.
;; Although this shouldn't be hard. Look at the Todo list, there are
;; definitely many things this script could do nicely to make a better
;; IRC experience for anyone.
;;
;; More info? Read the code. It isn't that complicated.
;;

;;; Installation:

;; Put erc.el and erc-speak.el somewhere in your load-path and
;; (require 'erc-speak) in your .emacs. Remember to require only erc-speak
;; because otherwise you get conflicts with emacspeak.

;;; Bugs:

;; erc-speak-rate doesn't seem to work here on outloud. Can anyone enlighten
;; me on the use of dtk-interp-queue-set-rate or equivalent?

;;; Code:

(require 'emacspeak)
(provide 'emacspeak-erc)
(require 'erc)
(require 'erc-button)

(defgroup erc-speak nil
  "Enable speech synthesis with the ERC chat client using Emacspeak"
  :group 'erc)

(defcustom erc-speak-personalities '((erc-default-face paul)
				     (erc-direct-msg-face paul-animated)
				     (erc-input-face paul-smooth)
				     (erc-bold-face paul-bold)
				     (erc-inverse-face betty)
				     (erc-underline-face ursula)
				     (erc-prompt-face harry)
				     (erc-notice-face paul-italic)
				     (erc-action-face paul-monotone)
				     (erc-error-face kid)
				     (erc-dangerous-host-face paul-surprized)
				     (erc-pal-face paul-animated)
				     (erc-fool-face paul-angry)
				     (erc-keyword-face paul-animated))
  "Maps faces used in erc to speaker personalities in emacspeak."
  :group 'erc-speak
  :type '(repeat
	  (list :tag "mapping"
		(symbol :tag "face")
		(symbol :tag "personality"))))

(add-hook 'erc-mode-hook (lambda () (setq voice-lock-mode t)))

;; Override the definition in erc.el
(defun erc-put-text-property (start end property value &optional object)
  "This function sets the appropriate personality on the specified
region in addition to setting the requested face."
  (put-text-property start end property value object)
  (when (eq property 'face)
    (put-text-property start end
		       'personality
		       (cadr (assq value erc-speak-personalities))
		       object)))

(add-hook 'erc-insert-post-hook 'erc-speak-region)
(add-hook 'erc-send-post-hook 'erc-speak-region)

(defcustom erc-speak-filter-host t
  "Set to t if you want to filter out user@host constructs."
  :group 'erc-speak
  :type 'bool)

(defcustom erc-speak-filter-timestamp t
  "If non-nil, try to filter out the timestamp when speaking arriving messages.

Note, your erc-timestamp-format variable needs to start with a [
and end with ]."
  :group 'erc-speak
  :type 'bool)

(defcustom erc-speak-acronyms '(("brb" "be right back")
				("btw" "by the way")
				("wtf" "what the fuck")
				("rotfl" "rolling on the floor and laughing")
				("afaik" "as far as I know")
				("afaics" "as far as I can see")
				("iirc" "if I remember correctly"))
  "List of acronyms to expand."
  :group 'erc-speak
  :type '(repeat sexp))

(defun erc-speak-acronym-replace (string)
  "Replace acronyms in the current buffer."
  (let ((case-fold-search nil))
    (dolist (ac erc-speak-acronyms string)
      (while (string-match (car ac) string)
	(setq string (replace-match (cadr ac) nil t string))))))

(defcustom erc-speak-smileys '((":-)" "smiling face")
			       (":)" "smiling face")
			       (":-(" "sad face")
			       (":(" "sad face"))
;; please add more, send me patches, mlang@home.delysid.org tnx
  "List of smileys and their textual description."
  :group 'erc-speak
  :type '(repeat (list 'symbol 'symbol)))

(defcustom erc-speak-smiley-personality 'harry
  "Personality used for smiley announcements."
  :group 'erc-speak
  :type 'symbol)

(defun erc-speak-smiley-replace (string)
  "Replace smileys with textual description."
  (let ((case-fold-search nil))
    (dolist (smiley erc-speak-smileys string)
      (while (string-match (car smiley) string)
	(let ((repl (cadr smiley)))
	  (put-text-property 0 (length repl) 'personality
			     erc-speak-smiley-personality repl)
	  (setq string (replace-match repl nil t string)))))))

(defcustom erc-speak-channel-personality 'harry
  "*Personality to announce channel names with."
  :group 'erc-speak
  :type 'symbol)

(defun erc-speak-region ()
  "Speak a region containing one IRC message using Emacspeak.
This function tries to translate common IRC forms into
intelligent speech."
  (let ((target (if (erc-channel-p (erc-default-target))
		    (erc-propertize
		     (erc-default-target)
		     'personality erc-speak-channel-personality)
		  ""))
	(dtk-stop-immediately nil))
    (emacspeak-auditory-icon 'progress)
    (when erc-speak-filter-timestamp
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^\\[[a-zA-Z:,;.0-9 \t-]+\\]" nil t)
	  (narrow-to-region (point) (point-max)))))
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward (concat "^<\\([^>]+\\)> "
					(concat "\\("
						erc-valid-nick-regexp
						"\\)[;,:]")) nil t)
	     (let ((from (match-string 1))
		   (to (match-string 2))
		   (text (buffer-substring (match-end 2) (point-max))))
	       (tts-with-punctuations
		"some"
		(dtk-speak (concat (erc-propertize
				    (concat target " " from " to " to)
				    'personality erc-speak-channel-personality)
				   (erc-speak-smiley-replace
				    (erc-speak-acronym-replace text)))))))
	    ((re-search-forward "^<\\([^>]+\\)> " nil t)
	     (let ((from (match-string 1))
		   (msg (buffer-substring (match-end 0) (point-max))))
	       (tts-with-punctuations
		"some"
		(dtk-speak (concat target " " from " "
				   (erc-speak-smiley-replace
				    (erc-speak-acronym-replace msg)))))))
	    ((re-search-forward (concat "^" (regexp-quote erc-notice-prefix)
					"\\(.+\\)")
				(point-max) t)
	     (let ((notice (buffer-substring (match-beginning 1) (point-max))))
	       (tts-with-punctuations
		"all"
		(dtk-speak
		 (with-temp-buffer
		   (insert notice)
		   (when erc-speak-filter-host
		     (goto-char (point-min))
		     (when (re-search-forward "([^)@]+@[^)@]+)" nil t)
		       (replace-match "")))
		   (buffer-string))))))
	    (t (let ((msg (buffer-substring (point-min) (point-max))))
		 (tts-with-punctuations
		  "some"
		  (dtk-speak (concat target " "
				     (erc-speak-smiley-replace
				      (erc-speak-acronym-replace msg)))))))))))

(provide 'erc-speak)

;;; erc-speak.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

;; arch-tag: 4499cd13-2829-43b8-83de-d313481531c4
