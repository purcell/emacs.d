;; Org mode settings

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org-install)
(require 'ob-ditaa)
(require 'org-latex)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; 默认以缩进方式打开 org 文件
(setq org-startup-indented t)

;; 用 listing 宏包格式化代码
(setq org-export-latex-listings t)

;; Options for \lset
(setq org-export-latex-listings-options
      '(("basicstyle" "\\color{foreground}\\tiny\\mono") ; 源代码字体样式
        ("keywordstyle" "\\color{function}\\bfseries\\tiny\\mono") ; 关键词字体样式
        ("identifierstyle" "\\color{doc}\\tiny\\mono")
        ("commentstyle" "\\color{comment}\\tiny\\itshape") ; 批注样式
        ("stringstyle" "\\color{string}\\tiny") ; 字符串样式
        ("showstringspaces" "false")          ; 字符串空格显示
        ("numbers" "none")                    ; 不显示行号
        ("backgroundcolor" "\\color{background}") ; 代码框背景色
        ("tabsize" "4")           ; 等效空格数TAB
        ("captionpos" "t")        ; 标题位置top or buttom(t|b)
        ("breaklines" "true")     ; 自动断行
        ("breakatwhitespace" "true") ; 只在空格分行
        ("showspaces" "false")       ; 显示空格
        ("columns" "flexible") ; 列样式
        ("frame" "single") ; 代码框：阴影盒
        ("frameround" "tttt") ; 代码框：圆角
        ("framesep" "0pt")
        ("framerule" "3pt")
        ("rulecolor" "\\color{background}")
        ("fillcolor" "\\color{white}")
        ("rulesepcolor" "\\color{comdil}")
        ("framexleftmargin" "5mm")
        ("xleftmargin" "5mm")
        ("escapeinside" "{(*@}{@*)}")
        ))

;; 执行两遍 xelatex 生成 PDF，以便正常产生含中文的目录
(setq org-latex-to-pdf-process
      '("xelatex -interaction=nonstopmode %f"
        "xelatex -interaction=nonstopmode %f"))

;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-completion-use-ido t)

;; 执行免应答
(setq org-confirm-babel-evaluate nil)

;; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

;; Default directory to look up Org files.
;;(setq org-directory "~/mydata/org/")

;; Record time when todo is done
(setq org-log-done 'time)
;;(setq org-agenda-files '("~/mydata/org/work.org" "~/mydata/org/home.org"))

;; flyspell mode for spell checking everywhere
;;(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Use current window for agenda
(setq org-agenda-window-setup 'current-window)

;; ditaa path 可考虑换成 DitaaEps
(setq org-ditaa-jar-path "~/jar/ditaa0_9.jar")

;; PlantUML path
(setq org-plantuml-jar-path "~/jar/plantuml.jar")

;; 加载各种语言的支持Babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '((R . t)
                               (emacs-lisp . t)
                               (matlab . t)
                               (C . t)
                               (perl . t)
                               (sh . t)
                               (ditaa . t)
                               (python . t)
                               (haskell . t)
                               (dot . t)
                               (latex . t)
                               (js . t)
                               (ruby . t)
                               (gnuplot . t)
                               (clojure . t)
                               (ledger . t)
                               (org . t)
                               (plantuml . t)))

;; LaTeX export
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

;; For export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass[12pt, a4paper]{article}
\\usepackage{fontspec, xunicode, xltxtra}
\\usepackage[slantfont, boldfont]{xeCJK} % 允许斜体和粗体
\\usepackage[xetex, colorlinks=true, CJKbookmarks=true, urlcolor=red, linkcolor=blue, menucolor=blue]{hyperref}

\\setCJKmainfont{WenQuanYi Micro Hei} % 默认中文字体
\\setCJKmonofont{WenQuanYi Micro Hei Mono} % 中文等宽字体
\\setmainfont{TeX Gyre Pagella} % 英文衬线字体
\\setmonofont{Monaco} % 英文等宽字体
\\setsansfont{Trebuchet MS} % 英文无衬线字体
\\punctstyle{kaiming} % 开明式标点格式: 句末点号用全角, 其他半角

% 定义\\mono字体命令供listings宏配置使用
\\newcommand\\fontnamemono{Monaco}
\\newfontinstance\\MONO{\\fontnamemono}
\\newcommand{\\mono}[1]{{\\MONO #1}}

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
%\\usepackage{algorithm}
%\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}

\\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}

\\definecolor{foreground}{RGB}{220,220,204}
\\definecolor{background}{RGB}{62,62,62}
\\definecolor{preprocess}{RGB}{250,187,249}
\\definecolor{var}{RGB}{239,224,174}
\\definecolor{string}{RGB}{154,150,230}
\\definecolor{type}{RGB}{225,225,116}
\\definecolor{function}{RGB}{140,206,211}
\\definecolor{keyword}{RGB}{239,224,174}
\\definecolor{comment}{RGB}{180,98,4}
\\definecolor{doc}{RGB}{175,215,175}
\\definecolor{comdil}{RGB}{111,128,111}
\\definecolor{constant}{RGB}{220,162,170}
\\definecolor{buildin}{RGB}{127,159,127}

\\pagestyle{fancy}
\\chead{\\MakeUppercase\\sectionmark}
\\fancyfoot[C]{\\bfseries\\thepage} % 页脚居中显示页码

\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; allow for export to beamer by placing
;; #+LaTeX_CLASS: beamer in org files
(add-to-list 'org-export-latex-classes
             '("beamer"
               "\\documentclass[11pt, professionalfonts, nofontenc]{beamer}
\\usepackage{fontspec, xunicode, xltxtra}
\\usepackage[slantfont, boldfont]{xeCJK} % 允许斜体和粗体
\\usepackage[xetex, colorlinks=true, CJKbookmarks=true, urlcolor=red, linkcolor=blue, menucolor=blue]{hyperref}

\\beamertemplateballitem
\\setbeameroption{show notes}

\\setCJKmainfont{WenQuanYi Micro Hei} % 默认中文字体
\\setCJKmonofont{WenQuanYi Micro Hei Mono} % 中文等宽字体
\\setmainfont{TeX Gyre Pagella} % 英文衬线字体
\\setmonofont{Monaco} % 英文等宽字体
\\setsansfont{Trebuchet MS} % 英文无衬线字体
\\punctstyle{kaiming} % 开明式标点格式: 句末点号用全角, 其他半角

% 定义\\mono字体命令供listings宏配置使用
\\newcommand\\fontnamemono{Monaco}
\\newfontinstance\\MONO{\\fontnamemono}
\\newcommand{\\mono}[1]{{\\MONO #1}}

\\usepackage{graphicx}
\\usepackage{tikz}
\\usepackage{xcolor}
\\usepackage{amsmath}
\\usepackage{lmodern}
\\usepackage{polyglossia}
\\usepackage{verbatim}
\\usepackage{listings}

\\definecolor{foreground}{RGB}{220,220,204}
\\definecolor{background}{RGB}{62,62,62}
\\definecolor{preprocess}{RGB}{250,187,249}
\\definecolor{var}{RGB}{239,224,174}
\\definecolor{string}{RGB}{154,150,230}
\\definecolor{type}{RGB}{225,225,116}
\\definecolor{function}{RGB}{140,206,211}
\\definecolor{keyword}{RGB}{239,224,174}
\\definecolor{comment}{RGB}{180,98,4}
\\definecolor{doc}{RGB}{175,215,175}
\\definecolor{comdil}{RGB}{111,128,111}
\\definecolor{constant}{RGB}{220,162,170}
\\definecolor{buildin}{RGB}{127,159,127}

[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; Use evince to open PDF files
(eval-after-load "org"
  '(progn
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(provide 'my-org-settings)
