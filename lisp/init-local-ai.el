;;; Package --- ai settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t
  :config (require 'gptel-org))

(setq
 gptel-model "sone-gemini-2.5-flash-think-0"
 gptel-backend
 (gptel-make-openai "sone"
   :host "sone.19982002.xyz"
   :key #'gptel-api-key-from-auth-source
   :models '("sone-gemini-2.5-flash-think-0"
             "sone-gemini-2.5-flash"
             "sone-gemini-2.0-flash-think-0"
             "sone-gemini-2.5-flash"
             "sone-gemini-2.5-pro-search")
   :stream t)

 gptel-backends
 (list gptel-backend
       (gptel-make-perplexity "Perplexity"
         :host "api.perplexity.ai"
         :key #'gptel-api-key-from-auth-source
         :models '("sonar")
         :stream t)))

(require 'init-local-gptel-proof nil t)

(provide 'init-local-ai)
;;; init-local-ai.el ends here
