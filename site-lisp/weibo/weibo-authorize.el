;; Copyright (C) 2011 Austin<austiny.cn@gmail.com>
          
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'url)

(defconst weibo-authorize-cb-url "http://127.0.0.1:42012/")
(defconst weibo-authorize-cb-server "weibo.emacs.cb")
(defconst weibo-authorize-url "https://api.weibo.com/oauth2/authorize?client_id=%s&response_type=token&redirect_uri=%s")
(defvar weibo-consumer-key "214135744")
(defvar weibo-consumer-secret "1e0487b02bae1e0df794ebb665d12cf6")

(defun weibo-authorize-cb-filter (proc string)
  (set-process-coding-system proc 'utf-8 'utf-8)
  (process-send-string proc "HTTP/1.0 200 OK\nContent-Type: text/html\n\n")
  (process-send-string proc "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html\"; charset=\"utf-8\"><script type=\"text/javascript\" > function load_at() {var hash = document.location.hash.substring(1); var result = \"\"; var token=\"\"; var expire=\"\"; var params = hash.split(\"&\"); for (var i = 0; i < params.length; i ++) {var pairs = params[i].split(\"=\"); if (pairs[0] == \"access_token\") token=pairs[1]; if (pairs[0] == \"expires_in\") expire=pairs[1];} document.getElementById(\"access\").innerHTML = token + \":\" + expire;} window.onload=load_at; </script></head><body>emacs.weibo提示您，您的授权码是：<div style=\"border:1px solid;\" id=\"access\"></div>请将框中的字符粘贴回emacs中</html>")
  (process-send-eof proc))

(defun weibo-authorize-start-cb-server ()
  (weibo-authorize-stop-cb-server)
  (make-network-process
    :name weibo-authorize-cb-server
    :service 42012
    :server t
    :family 'ipv4
    :filter 'weibo-authorize-cb-filter))

(defun weibo-authorize-stop-cb-server ()
  (when (process-status weibo-authorize-cb-server)
    (delete-process weibo-authorize-cb-server)))

(defun weibo-authorize-app ()
  (weibo-authorize-start-cb-server)
  (let ((auth-url (format weibo-authorize-url (url-hexify-string weibo-consumer-key) (url-hexify-string weibo-authorize-cb-url))))
    (browse-url auth-url)
    (let ((access-token (read-string (format "请输入授权码(如果浏览器没有自动打开，请访问 %s 获得授权码)：" auth-url))))
      (weibo-authorize-stop-cb-server)
      access-token)))

(provide 'weibo-authorize)