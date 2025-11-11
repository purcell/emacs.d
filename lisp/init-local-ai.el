;;; Package --- ai settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t
  :config (require 'gptel-org)

  (setq
   gptel-default-mode 'org-mode
   ;; gptel-model 'openrouter:openai/gpt-4o-mini
   gptel-highlight-mode t
   gptel-backend
   (gptel-make-openai "openrouter"
     :host "openrouter.ai"
     :endpoint "/api/v1/chat/completions"
     :stream t
     :key #'gptel-api-key-from-auth-source
     :models '("openai/gpt-4o-mini"
               "openai/gpt-4.1"
               "google/gemini-2.0-flash-exp:free"
               "google/gemini-2.5-pro"
               "anthropic/claude-sonnet-4.5"
               "minimax/minimax-m2:free"))

   gptel-backends
   (list gptel-backend
         (gptel-make-perplexity "perplexity"
           :host "api.perplexity.ai"
           :endpoint "/chat/completions"
           :stream t
           :key #'gptel-api-key-from-auth-source
           :models '(sonar))

         (gptel-make-openai "sone"
           :host "sone.19982002.xyz"
           :stream t
           :key #'gptel-api-key-from-auth-source
           :models '("sone-gemini-2.5-flash-think-0"
                     "sone-gemini-2.5-flash"
                     "sone-gemini-2.0-flash-think-0"
                     "sone-gemini-2.0-flash"
                     "sone-gemini-2.5-pro-search")))))

;; The chats can have long lines.
(add-hook 'gptel-mode-hook 'visual-line-mode)

;; And can be pages long.
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)

(defun gjg/gptel-backend-and-model ()
  "Return gptel backend and model (if any)."
  (let ((backend (if  (boundp 'gptel-backend)  (aref gptel-backend 1)))
        (model (if  (boundp 'gptel-model) gptel-model)))
    (format "(%s %s)" backend model)))

(defun gjg/gptel-insert-model-in-non-gptel-buffers ()
  "This function will add the backend and model in the \"dynamic\" buffers, not in dedicated chat buffers.
To be used in `gptel-pre-response-hook'."
  (unless (member 'gptel-mode local-minor-modes)
    (goto-char (point-max))
    (insert (format "\n%s: " (gjg/gptel-backend-and-model)))
    (goto-char (point-max))))
(add-hook 'gptel-pre-response-hook 'gjg/gptel-insert-model-in-non-gptel-buffers)

(defun gjg/gptel-insert-model-in-chat-buffers (response-begin-pos response-end-pos)
  "This function adds the backend and model in dedicated chat buffers.
Can be used with the `gptel-post-response-functions' hook."
  (let* ((gptel-org-prefix (alist-get 'org-mode gptel-prompt-prefix-alist))
         (inserted-string (format "%s %s\n"
                                  (substring gptel-org-prefix 0 (string-match " " gptel-org-prefix))
                                  (gjg/gptel-backend-and-model)))
         (len-inserted (length inserted-string )))
    (goto-char response-begin-pos)
    (insert inserted-string)
    (goto-char (+ response-end-pos len-inserted))))
(add-hook 'gptel-post-response-functions 'gjg/gptel-insert-model-in-chat-buffers)

(use-package gptel-prompts
  :vc (:url "https://github.com/jwiegley/gptel-prompts" :rev :newest)
  :after gptel
  :demand t
  :config
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(require 'init-local-gptel-proof nil t)

(provide 'init-local-ai)
;;; init-local-ai.el ends here
