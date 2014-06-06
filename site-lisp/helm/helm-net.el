;;; helm-net.el --- helm browse url and search web. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'url)
(require 'xml)
(require 'browse-url)


(defgroup helm-net nil
  "Net related applications and libraries for Helm."
  :group 'helm)

(defcustom helm-google-suggest-default-browser-function nil
  "The browse url function you prefer to use with google suggest.
When nil, use the first browser function available
See `helm-browse-url-default-browser-alist'."
  :group 'helm-net
  :type 'symbol)

(defcustom helm-home-url "http://www.google.fr"
  "Default url to use as home url."
  :group 'helm-net
  :type 'string)

(defcustom helm-surfraw-default-browser-function nil
  "The browse url function you prefer to use with surfraw.
When nil, fallback to `browse-url-browser-function'."
  :group 'helm-net
  :type 'symbol)

(defcustom helm-google-suggest-url
  "http://google.com/complete/search?output=toolbar&q="
  "URL used for looking up Google suggestions."
  :type 'string
  :group 'helm-net)

(defcustom helm-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for Google searching."
  :type 'string
  :group 'helm-net)

(defcustom helm-google-suggest-use-curl-p nil
  "When non--nil use CURL to get info from `helm-google-suggest-url'.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'helm-net)

(defcustom helm-yahoo-suggest-url
  "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=Generic&query="
  "Url used for looking up Yahoo suggestions."
  :type 'string
  :group 'helm-net)

(defcustom helm-yahoo-suggest-search-url
  "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
  "Url used for Yahoo searching."
  :type 'string
  :group 'helm-net)

(defcustom helm-surfraw-duckduckgo-url
  "https://duckduckgo.com/lite/?q=%s&kp=1"
  "The duckduckgo url.
This is a format string, don't forget the `%s'.
If you have personal settings saved on duckduckgo you should have
a personal url, see your settings on duckduckgo."
  :type 'string
  :group 'helm-net)

(defcustom helm-wikipedia-suggest-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&search="
  "Url used for looking up Wikipedia suggestions."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-wikipedia-url
  "https://en.wikipedia.org/wiki/Special:Search?search=%s"
  "The Wikipedia search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-wikipedia-summary-url
  "http://en.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page="
  "URL for getting the summary of a Wikipedia topic."
  :type 'string
  :group 'helm-net)

(defcustom helm-wikipedia-follow-delay 2
  "Delay before wikipedia summary popup."
  :type 'number
  :group 'helm-net)

(defcustom helm-search-suggest-action-youtube-url
  "http://www.youtube.com/results?aq=f&search_query=%s"
  "The Youtube search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-imdb-url
  "http://www.imdb.com/find?s=all&q=%s"
  "The IMDb search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-google-maps-url
  "http://maps.google.com/maps?f=q&source=s_q&q=%s"
  "The Google Maps search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-google-news-url
  "http://www.google.com/search?safe=off&prmd=nvlifd&source=lnms&tbs=nws:1&q=%s"
  "The Google News search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)


;;; Additional actions for search suggestions
;;
;;
;; Internal

(defun helm-search-suggest-perform-additional-action (url query)
  "Perform the search via URL using QUERY as input."
  (browse-url (format url (url-hexify-string query))))

(defvar helm-search-suggest-additional-actions
  '(("Wikipedia" . (lambda (candidate)
                     (helm-search-suggest-perform-additional-action
                      helm-search-suggest-action-wikipedia-url
                      candidate)))
    ("Youtube" . (lambda (candidate)
                   (helm-search-suggest-perform-additional-action
                    helm-search-suggest-action-youtube-url
                    candidate)))
    ("IMDb" . (lambda (candidate)
                (helm-search-suggest-perform-additional-action
                 helm-search-suggest-action-imdb-url
                 candidate)))
    ("Google Maps" . (lambda (candidate)
                       (helm-search-suggest-perform-additional-action
                        helm-search-suggest-action-google-maps-url
                        candidate)))
    ("Google News" . (lambda (candidate)
                       (helm-search-suggest-perform-additional-action
                        helm-search-suggest-action-google-news-url
                        candidate))))
  "List of additional actions for suggest sources.")


;;; Google Suggestions
;;
;;
;; Internal
(defvar helm-ggs-max-length-real-flag 0)
(defvar helm-ggs-max-length-num-flag 0)

(defun helm-google-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (setq helm-ggs-max-length-real-flag 0
        helm-ggs-max-length-num-flag 0)
  (let ((request (concat helm-google-suggest-url
                         (url-hexify-string input)))
        (fetch #'(lambda ()
                   (cl-loop
                         with result-alist = (xml-get-children
                                              (car (xml-parse-region
                                                    (point-min) (point-max)))
                                              'CompleteSuggestion)
                         for i in result-alist
                         for data = (cdr (cl-caadr (assoc 'suggestion i)))
                         for nqueries = (cdr (cl-caadr (assoc 'num_queries i)))
                         for lqueries = (length (helm-ggs-set-number-result
                                                 nqueries))
                         for ldata = (length data)
                         do
                         (progn
                           (when (> ldata helm-ggs-max-length-real-flag)
                             (setq helm-ggs-max-length-real-flag ldata))
                           (when (> lqueries helm-ggs-max-length-num-flag)
                             (setq helm-ggs-max-length-num-flag lqueries)))
                         collect (cons data nqueries) into cont
                         finally return cont))))
    (if helm-google-suggest-use-curl-p
        (with-temp-buffer
          (call-process "curl" nil t nil request)
          (funcall fetch))
      (with-current-buffer
          (url-retrieve-synchronously request)
        (funcall fetch)))))

(defun helm-google-suggest-set-candidates (&optional request-prefix)
  "Set candidates with result and number of google results found."
  (let ((suggestions
         (cl-loop with suggested-results = (helm-google-suggest-fetch
                                            (or (and request-prefix
                                                     (concat request-prefix
                                                             " " helm-pattern))
                                                helm-pattern))
               for (real . numresult) in suggested-results
               ;; Prepare number of results with ","
               for fnumresult = (helm-ggs-set-number-result numresult)
               ;; Calculate number of spaces to add before fnumresult
               ;; if it is smaller than longest result
               ;; `helm-ggs-max-length-num-flag'.
               ;; e.g 1,234,567
               ;;       345,678
               ;; To be sure it is aligned properly.
               for nspaces = (if (< (length fnumresult)
                                    helm-ggs-max-length-num-flag)
                                 (- helm-ggs-max-length-num-flag
                                    (length fnumresult))
                               0)
               ;; Add now the spaces before fnumresult.
               for align-fnumresult = (concat (make-string nspaces ? )
                                              fnumresult)
               for interval = (- helm-ggs-max-length-real-flag
                                 (length real))
               for spaces   = (make-string (+ 2 interval) ? )
               for display = (format "%s%s(%s results)"
                                     real spaces align-fnumresult)
               collect (cons display real))))
    (if (cl-loop for (_disp . dat) in suggestions
              thereis (equal dat helm-pattern))
        suggestions
      ;; if there is no suggestion exactly matching the input then
      ;; prepend a Search on Google item to the list
      (append
       suggestions
       (list (cons (concat "Search for " "'" helm-input "'" " on Google")
                   helm-input))))))

(defun helm-ggs-set-number-result (num)
  (if num
      (progn
        (and (numberp num) (setq num (number-to-string num)))
        (cl-loop for i in (reverse (split-string num "" t))
              for count from 1
              append (list i) into C
              when (= count 3)
              append (list ",") into C
              and do (setq count 0)
              finally return
              (replace-regexp-in-string
               "^," "" (mapconcat 'identity (reverse C) ""))))
    "?"))

(defun helm-google-suggest-action (candidate)
  "Default action to jump to a google suggested candidate."
  (let ((arg (concat helm-google-suggest-search-url
                     (url-hexify-string candidate))))
    (helm-aif helm-google-suggest-default-browser-function
        (funcall it arg)
      (helm-browse-url arg))))

(defvar helm-google-suggest-default-function
  'helm-google-suggest-set-candidates
  "Default function to use in helm google suggest.")

(defvar helm-source-google-suggest
  `((name . "Google Suggest")
    (candidates . (lambda ()
                    (funcall helm-google-suggest-default-function)))
    (action . ,(cons '("Google Search" . helm-google-suggest-action)
                     helm-search-suggest-additional-actions))
    (volatile)
    (keymap . ,helm-map)
    (requires-pattern . 3)))

(defun helm-google-suggest-emacs-lisp ()
  "Try to emacs lisp complete with google suggestions."
  (helm-google-suggest-set-candidates "emacs lisp"))


;;; Yahoo suggestions
;;
;;
(defun helm-yahoo-suggest-fetch (input)
  "Fetch Yahoo suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat helm-yahoo-suggest-url
                         (url-hexify-string input))))
    (with-current-buffer
        (url-retrieve-synchronously request)
      (cl-loop with result-alist =
            (xml-get-children
             (car (xml-parse-region
                   (point-min) (point-max)))
             'Result)
            for i in result-alist
            collect (cl-caddr i)))))

(defun helm-yahoo-suggest-set-candidates ()
  "Set candidates with Yahoo results found."
  (let ((suggestions (helm-yahoo-suggest-fetch helm-input)))
    (or suggestions
        (append
         suggestions
         (list (cons (concat "Search for " "'" helm-input "'" " on Yahoo")
                     helm-input))))))

(defun helm-yahoo-suggest-action (candidate)
  "Default action to jump to a Yahoo suggested candidate."
  (helm-browse-url (concat helm-yahoo-suggest-search-url
                           (url-hexify-string candidate))))

(defvar helm-source-yahoo-suggest
  `((name . "Yahoo Suggest")
    (candidates . helm-yahoo-suggest-set-candidates)
    (action . (("Yahoo Search" . helm-yahoo-suggest-action)))
    (volatile)
    (keymap . ,helm-map)
    (requires-pattern . 3)))

;;; Wikipedia suggestions
;;
;;
(declare-function json-read-from-string "json" (string))
(defun helm-wikipedia-suggest-fetch ()
  "Fetch Wikipedia suggestions and return them as a list."
  (require 'json)
  (let ((request (concat helm-wikipedia-suggest-url
                         (url-hexify-string helm-pattern))))
    (if helm-google-suggest-use-curl-p
        (with-temp-buffer
          (call-process "curl" nil t nil request)
          (helm-wikipedia--parse-buffer))
      (with-current-buffer
          (url-retrieve-synchronously request)
        (helm-wikipedia--parse-buffer)))))

(defun helm-wikipedia--parse-buffer ()
  (goto-char (point-min))
  (when (re-search-forward "^\\[.+\\[\\(.*\\)\\]\\]" nil t)
    (cl-loop for i across (aref (json-read-from-string (match-string 0)) 1)
          collect i into result
          finally return (or result
                             (append
                              result
                              (list (cons (format "Search for '%s' on wikipedia"
                                                  helm-pattern)
                                          helm-pattern)))))))

(defvar helm-wikipedia--summary-cache (make-hash-table :test 'equal))
(defun helm-wikipedia-persistent-action (candidate)
  (unless (string= (format "Search for '%s' on wikipedia"
                           helm-pattern)
                   (helm-get-selection nil t))
    (message "Fetching summary from Wikipedia...")
    (let ((buf (get-buffer-create "*helm wikipedia summary*"))
          result mess)
      (while (progn
               (setq result (or (gethash candidate helm-wikipedia--summary-cache)
                                (puthash candidate
                                         (prog1
                                             (helm-wikipedia-fetch-summary candidate)
                                           (setq mess "Done"))
                                         helm-wikipedia--summary-cache)))
               (when (and result
                          (listp result))
                 (setq candidate (cdr result))
                 (message "Redirected to %s" candidate)
                 t)))
      (if (not result)
          (message "Error when getting summary.")
        (with-current-buffer buf
          (erase-buffer)
          (setq cursor-type nil)
          (insert result)
          (fill-region (point-min) (point-max))
          (goto-char (point-min)))
        (display-buffer buf)
        (message mess)))))


(defun helm-wikipedia-fetch-summary (input)
  (let* ((request (concat helm-wikipedia-summary-url (url-hexify-string input))))
    (if helm-google-suggest-use-curl-p
        (with-temp-buffer
          (call-process "curl" nil t nil request)
          (helm-wikipedia--parse-summary))
      (with-current-buffer
          (url-retrieve-synchronously request)
        (helm-wikipedia--parse-summary)))))


(defun helm-wikipedia--parse-summary ()
  (goto-char (point-min))
  (when (search-forward "{" nil t)
    (let ((result (cdr (assoc '*
                              (assoc 'text
                                     (assoc 'parse
                                            (json-read-from-string
                                             (buffer-substring-no-properties
                                              (1- (point)) (point-max)))))))))
      (when result
        (if (string-match "<span class=\"redirectText\"><a href=[^>]+>\\([^<]+\\)" result)
            (cons 'redirect (match-string 1 result))

          ;; find the beginning of the summary text in the result

          ;; check if there is a table before the summary and skip that
          (when (or (string-match "</table>\\(\n<div.*?</div>\\)?\n<p>" result)
                    ;; otherwise just find the first paragraph
                    (string-match "<p>" result))
            ;; remove cruft and do a simple formatting 
            (replace-regexp-in-string
             "Cite error: .*" ""
             (replace-regexp-in-string
              "&#160;" ""
              (replace-regexp-in-string
               "\\[[^\]]+\\]" ""
               (replace-regexp-in-string
                "<[^>]*>" ""
                (replace-regexp-in-string
                 "</p>\n<p>" "\n\n"
                 (substring result (match-end 0)))))))))))))


(defvar helm-source-wikipedia-suggest
  `((name . "Wikipedia Suggest")
    (candidates . helm-wikipedia-suggest-fetch)
    (action . (("Wikipedia" . (lambda (candidate)
                                (helm-search-suggest-perform-additional-action
                                 helm-search-suggest-action-wikipedia-url
                                 candidate)))))
    (persistent-action . helm-wikipedia-persistent-action)
    (volatile)
    (keymap . ,helm-map)
    (follow . 1)
    (follow-delay . ,helm-wikipedia-follow-delay)
    (requires-pattern . 3)))


;;; Web browser functions.
;;
;;
;; If default setting of `w3m-command' is not
;; what you want and you modify it, you will have to reeval
;; also `helm-browse-url-default-browser-alist'.

(defvar helm-browse-url-chromium-program "chromium-browser")
(defvar helm-browse-url-uzbl-program "uzbl-browser")
(defvar helm-browse-url-default-browser-alist
  `((,(or (and (boundp 'w3m-command) w3m-command)
          "/usr/bin/w3m") . w3m-browse-url)
    (,browse-url-firefox-program . browse-url-firefox)
    (,helm-browse-url-chromium-program . helm-browse-url-chromium)
    (,helm-browse-url-uzbl-program . helm-browse-url-uzbl)
    (,browse-url-kde-program . browse-url-kde)
    (,browse-url-gnome-moz-program . browse-url-gnome-moz)
    (,browse-url-mozilla-program . browse-url-mozilla)
    (,browse-url-galeon-program . browse-url-galeon)
    (,browse-url-netscape-program . browse-url-netscape)
    (,browse-url-mosaic-program . browse-url-mosaic)
    (,browse-url-xterm-program . browse-url-text-xterm)
    ("emacs" . eww-browse-url))
  "*Alist of \(executable . function\) to try to find a suitable url browser.")

(cl-defun helm-generic-browser (url name &rest args)
  "Browse URL with NAME browser."
  (let ((proc (concat name " " url)))
    (message "Starting %s..." name)
    (apply 'start-process proc nil name
           (append args (list url)))
    (set-process-sentinel
     (get-process proc)
     #'(lambda (process event)
         (when (string= event "finished\n")
           (message "%s process %s" process event))))))

(defun helm-browse-url-chromium (url)
  "Browse URL with google chrome browser."
  (interactive "sURL: ")
  (helm-generic-browser
   url helm-browse-url-chromium-program))

(defun helm-browse-url-uzbl (url &optional _ignore)
  "Browse URL with uzbl browser."
  (interactive "sURL: ")
  (helm-generic-browser url helm-browse-url-uzbl-program "-u"))

(defun helm-browse-url-default-browser (url &rest args)
  "Find the first available browser and ask it to load URL."
  (let ((default-browser-fn
         (cl-loop for (exe . fn) in helm-browse-url-default-browser-alist
               thereis (and exe (executable-find exe) (fboundp fn) fn))))
    (if default-browser-fn
        (apply default-browser-fn url args)
      (error "No usable browser found"))))

(defun helm-browse-url (url &rest args)
  "Default command to browse URL."
  (if browse-url-browser-function
      (browse-url url args)
    (helm-browse-url-default-browser url args)))


;;; Surfraw
;;
;; Need external program surfraw.
;; <http://surfraw.alioth.debian.org/>

;; Internal
(defvar helm-surfraw-engines-history nil)
(defvar helm-surfraw-input-history nil)
(defvar helm-surfraw--elvi-cache nil)

(defun helm-build-elvi-list ()
  "Return list of all engines and descriptions handled by surfraw."
  (or helm-surfraw--elvi-cache
      (setq helm-surfraw--elvi-cache
            (cdr (with-temp-buffer
                   (call-process "surfraw" nil t nil "-elvi")
                   (split-string (buffer-string) "\n"))))))

;;;###autoload
(defun helm-surfraw (pattern engine)
  "Preconfigured `helm' to search PATTERN with search ENGINE."
  (interactive (list (read-string "SearchFor: "
                                  nil 'helm-surfraw-input-history
                                  (thing-at-point 'symbol))
                     (helm-comp-read
                      "Engine: "
                      (helm-build-elvi-list)
                      :must-match t
                      :name "Surfraw Search Engines"
                      :del-input nil
                      :history helm-surfraw-engines-history)))
  (let* ((engine-nodesc (car (split-string engine)))
         (url (if (string= engine-nodesc "duckduckgo")
                  ;; "sr duckduckgo -p foo" is broken, workaround.
                  (format helm-surfraw-duckduckgo-url
                          (url-hexify-string pattern))
                (with-temp-buffer
                  (apply 'call-process "surfraw" nil t nil
                         (append  (list engine-nodesc "-p") (split-string pattern)))
                  (replace-regexp-in-string
                   "\n" "" (buffer-string)))))
         (browse-url-browser-function (or helm-surfraw-default-browser-function
                                          browse-url-browser-function)))
    (if (string= engine-nodesc "W")
        (helm-browse-url helm-home-url)
      (helm-browse-url url)
      (setq helm-surfraw-engines-history
            (cons engine (delete engine helm-surfraw-engines-history))))))

;;;###autoload
(defun helm-google-suggest ()
  "Preconfigured `helm' for google search with google suggest."
  (interactive)
  (helm-other-buffer 'helm-source-google-suggest "*helm google*"))

;;;###autoload
(defun helm-yahoo-suggest ()
  "Preconfigured `helm' for Yahoo searching with Yahoo suggest."
  (interactive)
  (helm-other-buffer 'helm-source-yahoo-suggest "*helm yahoo*"))

;;;###autoload
(defun helm-wikipedia-suggest ()
  "Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest."
  (interactive)
  (helm :sources 'helm-source-wikipedia-suggest
        :buffer "*helm wikipedia*"))


(provide 'helm-net)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-net.el ends here
