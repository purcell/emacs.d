;;; my-python.el --- My Python configs
;;; Commentary:
;;; Code:

(require 'dap-python)

(dap-register-debug-template
 "DevAutomator Testing"
 (list :type "python"
       :args ""
       :cwd nil
       :program "/home/ayden/Codebase/DevWork/DevAutomator/src/lang/TestCases"
       :module "pytest"
       :request "launch"
       :name "DevAutomator Testing"))

(provide 'my-python)
;;; my-python.el ends here
