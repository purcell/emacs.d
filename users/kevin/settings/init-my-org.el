;;; init-my-org
;;================================================================
;; Config for org table
;;================================================================

;; (eval-when-compile (require 'cl)) 

;; (defun set-font (english chinese english-size chinese-size) 
;;   (set-face-attribute 'default nil :font 
;;                       (format "%s:pixelsize=%d" english english-size)) 
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo)) 
;;     (set-fontset-font (frame-parameter nil 'font) charset 
;;                       (font-spec :family chinese :size chinese-size)))) 

;; (ecase system-type 
;;   (gnu/linux 
;;    (set-face-bold-p 'bold nil) 
;;    (set-face-underline-p 'bold nil) 
;;    (set-font "monofur" "vera Sans YuanTi Mono" 20 16)) 
;;   (darwin 
;;    (set-font "monofur" "STHeiti" 20 16)))



;;================================================================
;; Config for Org Capture
;;================================================================
;; config for org-mode
;; add some capture config
(setq org-directory "~/workspace/github/work-notes/captures")
;;(setq org-default-notes-file (concat org-directory "/notes.org"))
;;(define-key global-map (kbd "M-<f6>") 'org-capture)
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "TODO" entry (file (concat org-directory "/refile.org"))
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "RESPONED" entry  (file (concat org-directory "/refile.org"))
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "NOTES" entry  (file (concat org-directory "/refile.org"))
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry  (file (concat org-directory "/refile.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry  (file (concat org-directory "/refile.org"))
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry  (file (concat org-directory "/refile.org"))
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry  (file (concat org-directory "/refile.org"))
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry  (file (concat org-directory "/refile.org"))
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))



;;================================================================
;; Config for Refile
;;================================================================
;;


;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

;; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;================================================================
;; Config for Clock
;;================================================================
;;

;;================================================================
;; Config for Picture Drawing
;;================================================================
;; set for plantuml
(setq org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/plantuml.8031.jar")
;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;;================================================================
;; Config for TODO Configuration
;;================================================================
;; (setq org-todo-keywords
;;       (quote (;;(sequence "TODO(t)" "NEXT(n)" "MAYBE(m)" "STARTED(s)" "APPT(a)" "|" "DONE(d)")
;;               (sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d)")
;;               (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "MAYBE(m)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces
      (quote (;;("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face)
              ("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("STARTED" :foreground "green" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("MAYBE" :foreground "grey" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              )))


(setq org-use-fast-todo-selection t)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("MAYBE" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;;================================================================
;; Config for Tags
;;================================================================
;; Config TODO tags
(setq org-tag-alist '((:startgroup)
                      ("Develop" . ?1)
                      (:grouptags )
                      ("孔祥润" . ?a)
                      ("青文豪" . ?b)
                      ("郭春光" . ?c)
                      ("番梦琳" . ?d)
                      ("陈金牛" . ?e)
                      ("卞翠翠" . ?f)
                      ("鄢广顺" . ?g)
                      ("王帅" . ?h)
                      ("于伟豪" . ?i)
                      ("王培聪" . ?j)
                      ("周勇" . ?k)
                      ("任成祥" . ?l)
                      ("安龙" . ?m)
                      ("董晓霞" . ?n)
                      ("付飞武" . ?o)
                      ("付维" . ?p)
                      ("刘俊" . ?q)
                      ("刘丽红" . ?r)
                      ("刘真君" . ?s)
                      ("孙晓坤" . ?t)
                      ("张明轩" . ?u)
                      ("柴杰" . ?v)
                      ("张京仁" . ?w)
                      ("赵泽军" . ?x)
                      ("周尔耕" . ?y)
                      ("陆健美" . ?z)
                      (:endgroup)

                      (:startgroup)
                      ("Test" . ?2)
                      (:grouptags)
                      ("高玉谦" . ?A)
                      ("张家铭" . ?B)
                      ("宫𣫚" . ?C)
                      ("王美艳" . ?D)
                      (:endgroup . nil)

                      (:startgroup)
                      ("Servers" . ?3)
                      (:grouptags)
                      ("孙立臣" . ?E)
                      ("赵丽" . ?F)
                      (:endgroup)

                      (:startgroup)
                      ("Front" . ?4)
                      (:grouptags)
                      ("潘梦琳" . ?G)
                      ("刘品希" . ?H)
                      ("余虹君" . ?J)
                      (:endgroup)

                      (:startgroup)
                      ("Content" . ?5)
                      (:grouptags)
                      ("费洪阳" . ?K)
                      (:endgroup)

                      (:startgroup)
                      ("APP" . ?6)
                      (:grouptags)
                      ("李成龙" . ?L)
                      ("卢杨杨" . ?M)
                      (:endgroup)

                      (:startgroup)
                      ("Design" . ?7)
                      (:grouptags )
                      ("孙嘉蔚" . ?N)
                      (:endgroup)

                      ))
;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)


;;================================================================
;; Config for Global column view and properties
;;================================================================
;; Set default column view headings: Task Effort Clock_Summary
;;(setq org-columns-default-format "%25ITEM %10Effort(Effort){:} %SCHEDULED %DEADLINE %11Status %20TAGS %PRIORITY %TODO")
;;(setq org-columns-default-format "%25ITEM  %9Approved(Approved?){X} %SCHEDULED %DEADLINE %11Status %TAGS %PRIORITY %TODO")
(setq org-columns-default-format
      ;;" %TODO %30ITEM %15DEADLINE %15SCHEDULED %3PRIORITY %10TAGS %5Effort(Effort){:} %6CLOCKSUM"
      " %30FILE %TODO %30ITEM %15DEADLINE %15SCHEDULED %3PRIORITY %10TAGS %5Effort(Effort){:}"
      )

;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties (quote (
                                    ("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("Status_ALL" . "Not-start In-Progress Delay Finished Cancled")
                                    ("ID_ALL" . "")
                                    ("STYLE_ALL" . "habit"))))
;; update dynamic blocks when save file
(add-hook 'before-save-hook 'org-update-all-dblocks)



;;================================================================
;; Config for File Export HTML Format
;;================================================================ 

;; Increase default number of headings to export
(setq org-export-headline-levels 6)
;; List of projects
;; Work-notes

(setq org-publish-project-alist

      ;; (work notes for)
      (quote (("work-notes"
               :base-directory "~/workspace/github/work-notes/"
               :publishing-directory "~/workspace/github/publish-works"
               :recursive t
               :table-of-contents t
               :base-extension "org"
               :publishing-function org-html-publish-to-html
               :style-include-default t
               :section-numbers y
               :table-of-contents y
               :author-info y
               :creator-info y)
              ("work-notes-extra"
               :base-directory "~/workspace/github/work-notes/"
               :publishing-directory "~/workspace/github/publish-works"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("worknotes"
               :components ("work-notes" "work-notes-extra"))
              )))
;; set parent node into DONE when all sub-tasks are done in org mode
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; I'm lazy and don't want to remember the name of the project to publish when I modify
;; a file that is part of a project.  So this function saves the file, and publishes
;; the project that includes this file
;; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let (
        )
    (org-publish-current-project force)))
;; config for export-mutilpul files
(global-set-key (kbd "C-<f12>") 'bh/save-then-publish)

;;================================================================
;; Config for File Export HTML CSS Template
;;================================================================ 
;; add css for org-mode export to html files
;; Use org.css from the norang website for export document stylesheets
;;(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"./org.css\" type=\"text/css\" />")
(setq org-html-head-extra "<style type=\"text/css\">body{margin:
1em; border-right: 5px solid #bbb; border-bottom: 5px solid #bbb;
padding: 0; background: #ddd none repeat scroll 0 0; border: 1px
solid #000; margin: 0; padding: 2em; color: #000;
font-family: \"Bitstream Vera Sans\", Verdana, sans-serif;
font-size: 85%;}code{color: #00f;}div#content{border: 1px solid
#bbb; background: #fff; margin: 0; padding: 2em;}a{color: #139;
text-decoration: none; padding: 1px;}a:hover{color:
#900;}#table-of-contents{margin: 1em 0; padding:
.1em;}div#content div#org-div-home-and-up{background: #369;
color: #fff;}div#org-div-home-and-up
a:link,div#org-div-home-and-up a:visited{color: #fff; background:
#369;}div#org-div-home-and-up a:hover{color:
#900;}div.title{margin: -1em -1em 0; font-size: 200%;
font-weight: bold; background: #369; color: #fff; padding: .75em
1em; font-family: \"BitStream Vera Sans\", Verdana; letter-spacing:
.1em;}h1{background: #369 none repeat scroll 0 0; color: #fff;
font-family: \"BitStream Vera Sans\", Verdana; font-size: 200%;
font-weight: bold; letter-spacing: 0.1em; margin: -1em -1em .2em;
padding: 0.75em 1em;}h2{font-size: 180%; border-bottom: 1px solid
#ccc; padding: .2em;}h3{font-size: 120%; border-bottom: 1px solid
#eee;}h4{font-size: 110%; border-bottom: 1px solid
#eee;}tt{color: #00f;}.verbatim{margin: .5em 0;}pre{border: 1px
solid #ccc; background: #eee; padding: .5em; overflow:
auto;}.verbatim pre{margin: 0;}.verbatim-caption{border: 1px
solid #ccc; border-bottom: 0; background: #fff; display: block;
font-size: 80%; padding: .2em;}div#postamble{text-align: left;
color: #888; font-size: 80%; padding: 0; margin: 0;}div#postamble
p{padding: 0; margin: 0;}div#postamble a{color:
#888;}div#postamble a:hover{color: #900;}table{font-size: 100%;
border-collapse: collapse; margin: .5em 0;}th, td{border: 1px
solid #777; padding: .3em; margin: 2px;}th{background:
#eee;}span.underline{text-decoration:
underline;}.fixme{background: #ff0; font-weight:
bold;}.ra{text-align: right;}span.todo.NEXT{color:blue;}span.todo.STARTED{color:green;}span.todo.WAITTING{color:orange;}span.todo.HOLD{color:magenta;}.tag{font-size:124%;}</style>")



;;================================================================
;; Config for File Export To PDF 
;;================================================================ 
;; config for export PDF
(require 'ox-latex)
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))
;;(require 'org-install)

;; 使用xelatex一步生成PDF


(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
;; code执行免应答（Eval code without confirm）
(setq org-confirm-babel-evaluate nil)

;; Auctex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("cn-article"
               "\\documentclass[10pt,a4paper]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}

\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage{comment}

\\usepackage{zhfontcfg} % added my own sty file under /usr/local/texlive/texmf-local/tex/latex/local

\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\usepackage{xunicode,xltxtra}

\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}

\\XeTeXlinebreakskip = 0pt plus 1pt

\\definecolor{foreground}{RGB}{220,220,204}%浅灰
\\definecolor{background}{RGB}{62,62,62}%浅黑
\\definecolor{preprocess}{RGB}{250,187,249}%浅紫
\\definecolor{var}{RGB}{239,224,174}%浅肉色
\\definecolor{string}{RGB}{154,150,230}%浅紫色
\\definecolor{type}{RGB}{225,225,116}%浅黄
\\definecolor{function}{RGB}{140,206,211}%浅天蓝
\\definecolor{keyword}{RGB}{239,224,174}%浅肉色
\\definecolor{comment}{RGB}{180,98,4}%深褐色
\\definecolor{doc}{RGB}{175,215,175}%浅铅绿
\\definecolor{comdil}{RGB}{111,128,111}%深灰
\\definecolor{constant}{RGB}{220,162,170}%粉红
\\definecolor{buildin}{RGB}{127,159,127}%深铅绿
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-latex-listings t)
;; Options for \lset command（reference to listing Manual)
(setq org-latex-listings-options
      '(
        ("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
        ("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
        ("identifierstyle" "\\color{doc}\\small\\mono")
        ("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
        ("stringstyle" "\\color{string}\\small")                    ; 字符串样式
        ("showstringspaces" "false")                                ; 字符串空格显示
        ("numbers" "left")                                          ; 行号显示
        ("numberstyle" "\\color{preprocess}")                       ; 行号样式
        ("stepnumber" "1")                                          ; 行号递增
        ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
        ("tabsize" "4")                                             ; TAB等效空格数
        ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
        ("breaklines" "true")                                       ; 自动断行
        ("breakatwhitespace" "true")                                ; 只在空格分行
        ("showspaces" "false")                                      ; 显示空格
        ("columns" "flexible")                                      ; 列样式
        ("frame" "single")                                          ; 代码框：阴影盒
        ("frameround" "tttt")                                       ; 代码框： 圆角
        ("framesep" "0pt")
        ("framerule" "8pt")
        ("rulecolor" "\\color{background}")
        ("fillcolor" "\\color{white}")
        ("rulesepcolor" "\\color{comdil}")
        ("framexleftmargin" "10mm")
        ))
;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-completion-use-ido t)
;; 各种Babel语言支持
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (matlab . t)
   (C . t)
   (perl . t)
   (sh . t)
   (ditaa . t)
   (plantuml . t)
   (org . t)
   (python . t)
   (sh . t)
   (dot . t)
   (haskell . t)
   (dot . t)
   (latex . t)
   (java . t)
   (js . t)
   ))

;;================================================================
;; Config for File Export To PDF but use beamer
;;================================================================ 
;; 导出Beamer的设置
;; allow for export=>beamer by placing #+LaTeX_CLASS: beamer in org files
;;-----------------------------------------------------------------------------
(add-to-list 'org-latex-classes
             ;; beamer class, for presentations
             '("beamer"
               "\\documentclass[11pt,professionalfonts]{beamer}
\\mode
\\usetheme{{{{Warsaw}}}}
%\\usecolortheme{{{{beamercolortheme}}}}

\\beamertemplateballitem
\\setbeameroption{show notes}
\\usepackage{graphicx}
\\usepackage{tikz}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{amsmath}
\\usepackage{lmodern}
\\usepackage{fontspec,xunicode,xltxtra}
\\usepackage{polyglossia}

\\usepackage{verbatim}
\\usepackage{listings}
\\institute{{{{beamerinstitute}}}}
\\subject{{{{beamersubject}}}}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))



;; set org-article

(add-to-list 'org-latex-classes
             '("org-article"
               "\\documentclass{org-article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-default-class "cn-article")

;;
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;;================================================================
;; Config for Global function
;;================================================================
;; auto save files
(run-at-time "00:59" 3600 'org-save-all-org-buffers)





(provide 'init-my-orge)
;; init-my-org.el end here
