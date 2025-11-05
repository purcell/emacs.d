;;; Package --- ai settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t
  :config (require 'gptel-org))

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
   :models '("google/gemini-2.0-flash-exp:free"
             "google/gemini-2.5-pro"
             "openai/gpt-4o-mini"
             "openai/gpt-4.1"
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
                   "sone-gemini-2.5-flash"
                   "sone-gemini-2.5-pro-search"))))


(require 'init-local-gptel-proof nil t)

(provide 'init-local-ai)
;;; init-local-ai.el ends here
