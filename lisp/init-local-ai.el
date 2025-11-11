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
           :stream t
           :key #'gptel-api-key-from-auth-source
           :models '("sonar"))

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
