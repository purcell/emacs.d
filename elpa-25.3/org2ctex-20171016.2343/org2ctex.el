;;; org2ctex.el --- Export org to ctex (a latex macro for Chinese)

;; * Header
;; #+BEGIN_EXAMPLE
;; Copyright (c) 2017, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/org2ctex
;; Package-Version: 20171016.2343
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))

;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; #+END_EXAMPLE

;;; Commentary:

;; * README                                                             :README:
;; ** 简介
;; org2ctex 是一个 org-mode 工具，它可以帮助 org-mode 中文用户简单快速的
;; 配置 "org->latex->pdf" 工作环境。

;; 注意， 这个工具只是让 中文 pdf *基本正确* 的生成，如果你需要生成 *完美* 的中文 pdf，
;; 就需要花时间深入的了解下面工具：

;; 1. TeX 系统，比如：TexLive
;; 2. Ctex 宏包以及其相关工具（实现中文 LaTeX 的主要方式）
;; 3. ox-latex.el (org-mode 的 Latex 导出模块)

;; 另外需要用户了解的问题是：TeX 的引擎有许多种类，比如： pdftex, xetex 或者 luatex,
;; 每一种引擎都有特殊的中文设置，ox-latex 可以通过设置来支持上述所有 TeX 引擎,
;; 而 org2ctex *硬绑定* 到 xetex, *只支持 xetex* , 因为 xetex 可以
;; 通过 xft 来调用系统中的字体，中文配置比较容易，但 xetex 生成 pdf 的速度比较慢，
;; *我在这里告诉大家，不要因为 org2ctex 的方便而限制了自己的思路！*

;; [[./snapshots/org2ctex.gif]]

;; ** 下载并安装 Texlive
;; *** 下载 Tex Live ISO 镜像
;; TeX Live 支持 Linux，Windows 以及 MacOX 多种操作系统，安装方式多种多样，
;; 由于我这里网络不太稳定，所以我选择通过 Tex Live ISO 来安装 Tex Live （文件大小大约3G），

;; Tex Live 主站访问速度很慢，建议同学们使用国内镜像，许多 Linux 镜像站点都同时包含 CTAN。
;; 大家可以在 http://www.cnbeta.com/articles/194758.htm
;; 列出的镜像站点中寻找 “CTAN” 子目录，两个比较好用的 CTAN 镜像：

;; 1. 中国科学技术大学镜像：http://mirrors.ustc.edu.cn/CTAN/systems/texlive/Images/
;; 2. 清华大学镜像：http://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/Images/

;; *** 挂载已经下载的 ISO 镜像
;; 1. 下载安装 [[http://wincdemu.sysprogs.org/][wincdemu]], winmount 或者 DAEMON Tools，
;;    这类工具可以将 ISO 文件挂载到一个盘符，比直接解压缩快速方便。
;; 2. 将 ISO 镜像挂载到 “Z:\”

;; *** 运行安装程序
;; 1. 打开 “Z” 盘。
;; 2. *完全安装* 请运行 “install-tl.bat”，这种方式比较省心，但很占空间。
;; 3. *定制安装* 请运行 “install-tl-advanced.bat” ，这种方式相对麻烦，但节省安装时间和硬盘空间。

;; 注：安装的时候建议关闭杀毒软件。

;; *** 设置安装参数
;; 运行安装程序后，会弹出一个窗口（比较丑陋）用于设置安装选项，在 “选择安装方案” 选项中选择 “scheme-small”，
;; 其它选项不变，然后开始安装（安装大概需要 5 分钟左右）。

;; [[./snapshots/advanced-install.gif]]

;; *** 安装所需要的 Latex 宏包

;; **** 第一种方式：使用 Tex Live 管理器图形界面安装

;; [[./snapshots/install-latex-package-with-gui.gif]]

;; 1. 运行 Tex Live 管理器： 开始 > 程序 > Tex Live 2015 > Tex Live Manager
;; 2. 载入本地宏包仓库：tlmgr > 载入其它仓库，在弹出的对话框中选择 “choose local directory”，载入本地仓库 “Z:”。
;; 3. 安装所需宏包： collection-langcjk, collection-langchinese, ctex, ctex-faq, bibtex8, environ, trimspaces, zhnumber, wrapfig, capt-of, latexmk, dvipng, dvisvgm


;; **** 第二种方式：使用 tlmge 命令安装

;; 1. 运行 window CMD
;; 2. 设置 Latex 宏包仓库
;;    #+BEGIN_EXAMPLE
;;    tlmgr option repository Z:
;;    #+END_EXAMPLE
;; 3. 更新设置
;;    #+BEGIN_EXAMPLE
;;    tlmgr update -all
;;    #+END_EXAMPLE
;; 4. 安装所需宏包
;;    #+BEGIN_EXAMPLE
;;    tlmgr install collection-langcjk collection-langchinese ctex ctex-faq bibtex8 environ trimspaces zhnumber wrapfig capt-of latexmk dvipng dvisvgm
;;    #+END_EXAMPLE

;; ** 安装 org2ctex
;; 1. 配置 melpa: http://melpa.org/#/getting-started
;; 2. M-x package-install RET org2ctex RET

;; ** 配置 org2ctex
;; #+BEGIN_EXAMPLE
;; (require 'org2ctex)
;; (org2ctex-toggle t)
;; #+END_EXAMPLE

;; ** 高级设置

;; *** 如何选择默认 class
;; 设置变量 `org2ctex-latex-default-class', 默认有四个选项
;; 可以选择: "ctexart", "ctexrep", "ctexbook" 和 "beamer",
;; 用户可以通过设置 `org2ctex-latex-classes' 来添加更多的
;; class 配置。

;; *** 如何添加 Latex 宏包
;; 设置 `org2ctex-latex-packages-alist', 这个变量的设置方式和
;; `org-latex-packages-alist' 一样，请参考 org 手册。

;; *** 如何导出 org2ctex 的配置
;; 使用 `org2ctex-insert-configure-template' 在当前 buffer 的光标处
;; 插入一段 Emacs 配置代码, 这段 elisp 代码的功能和 org2ctex 的功能
;; *基本一样* , 用户可以把这段代码做为自己的 org 中文配置来进一步调整优化。

;; *** 如何禁用自动设置 LaTeX 字体功能
;; Org2ctex 默认会搜索当前系统 *可用* 的字体，来自动设置 LaTeX 字体，
;; 但字体显示效果未必好，用户可以使用下面的代码来禁用这个功能。

;; #+BEGIN_EXAMPLE
;; (setq org2ctex-latex-fonts nil)
;; #+END_EXAMPLE

;; ** 如何设置 LaTeX 片断预览功能
;; 1. 确保 Emacs 可以显示 png 文件，具体参考：ftp://ftp.gnu.org/gnu/emacs/windows/README
;; 2. 安装 [[http://www.imagemagick.org/][imagemagick]] 和 [[http://ghostscript.com/][ghostscript]]
;; 3. 设置 Emacs
;;    #+BEGIN_EXAMPLE
;;    ;; (setq org-latex-create-formula-image-program 'dvipng)    ;速度很快，但 *默认* 不支持中文
;;    (setq org-latex-create-formula-image-program 'imagemagick)  ;速度较慢，但支持中文
;;    (setq org-format-latex-options
;;          (plist-put org-format-latex-options :scale 2.0))      ;调整 LaTeX 预览图片的大小
;;    (setq org-format-latex-options
;;          (plist-put org-format-latex-options :html-scale 2.5)) ;调整 HTML 文件中 LaTeX 图像的大小
;;    #+END_EXAMPLE

;; ** 常见错误排查和解决

;; *** 我使用 MSYS2 内置的 Emacs, 无法找到 texlive 相关的命令

;; MSYS2 默认不读取 window 系统的 PATH 设置，用户可以更改
;; MSYS2 的快捷方式，为快捷方式添加 "-full-path" 参数
;; （右击快捷方式，选择 “属性”，更改 “目标”）

;; 比如：

;; #+BEGIN_EXAMPLE
;; "C:\msys32\msys2_shell.cmd -mingw32 -full-path"
;; #+END_EXAMPLE

;; *** 转换时字体名字乱码

;; 确保 org 源文件保存为 UTF-8 格式，GBK 格式的文件处理起来有问题。

;; *** 中文目录下的 org 文件无法转换为 pdf 文件
;; 这个问题可以使用 latexmk 命令配合 "%b.tex" (仅仅使用文件名，而不是文件的绝对路径) 来规避，比如：

;; #+BEGIN_EXAMPLE
;; (setq org2ctex-latex-commands '("latexmk -xelatex -gg -pdf %b.tex"))
;; #+END_EXAMPLE

;; *** 缺少必要的 Latex 宏包
;; **** 表现形式
;;  1. message buffer中有类似输出
;;     #+BEGIN_EXAMPLE
;;     Processing LaTeX file ./export-org-file-to-pdf-with-ctex.tex...
;;     PDF file ./export-org-file-to-pdf-with-ctex.pdf wasn't produced
;;     #+END_EXAMPLE
;;  2. *Org PDF LaTeX Output* buffer 中有类似输出
;;     #+BEGIN_EXAMPLE
;;     ...

;;     (c:/texlive/2015/texmf-dist/tex/generic/oberdiek/ltxcmds.sty)
;;     (c:/texlive/2015/texmf-dist/tex/generic/oberdiek/kvsetkeys.sty
;;     (c:/texlive/2015/texmf-dist/tex/generic/oberdiek/infwarerr.sty)
;;     (c:/texlive/2015/texmf-dist/tex/generic/oberdiek/etexcmds.sty
;;     (c:/texlive/2015/texmf-dist/tex/generic/oberdiek/ifluatex.sty))))
;;     (c:/texlive/2015/texmf-dist/tex/generic/oberdiek/pdftexcmds.sty))
;;     (c:/texlive/2015/texmf-dist/tex/latex/tools/longtable.sty)

;;     ! LaTeX Error: File `wrapfig.sty' not found.

;;     Type X to quit or <RETURN> to proceed,
;;     or enter new name.  (Default extension: sty)

;;     Enter file name:
;;     ! Emergency stop.
;;     <read *>

;;     l.8 \usepackage
;;                    {rotating}^^M
;;     No pages of output.

;;     ...
;;     #+END_EXAMPLE

;; **** 解决方式
;;  #+BEGIN_EXAMPLE
;;     ! LaTeX Error: File `wrapfig.sty' not found.
;;  #+END_EXAMPLE
;;  这个错误提示说明，你安装的 Tex Live 中没有包含 wrapfig.sty 这个文件，需要你安装，解决方式：

;;  1. 直接试着安装 “wrapfig” 宏包
;;     #+BEGIN_EXAMPLE
;;     tlmgr option repository Z:
;;     tlmgr update -all
;;     tlmgr install wrapfig
;;     #+END_EXAMPLE
;;  2. 如果没有找到 “wrapfig” 宏包，你需要找到哪个宏包包含 wrapfig.sty，一个简单的方式就是用 google 搜索 wrapfig.sty，一般会有相关的信息。

;;; Code:

;; * 代码                                                          :code:
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'cl-lib)
(require 'cl-extra)

(defgroup org-export-latex-chinese nil
  "Options for exporting Org mode files to LaTeX."
  :group 'org-export-latex)

(defcustom org2ctex-latex-fonts
  '((mainfont "Times New Roman")
    (CJKmainfont "SimSun" "STSong" "STZhongson" "宋体" "新宋体" "宋体" "华文中宋")
    (CJKmainfont-italic "KaiTi_GB2312" "KaiTi" "STKaiti" "楷体" "楷体_GB2312" "华文行楷")
    (CJKsansfont "Microsoft Yahei" "Microsoft_Yahei" "WenQuanYi Micro Hei" "SimHei"
                 "文泉驿微米黑" "文泉驿等宽微米黑" "微软雅黑" "文泉驿等宽正黑" "黑体"
                 "文泉驿正黑" "文泉驿点阵正黑" "华文细黑")
    (CJKmonofont "WenQuanYi Micro Hei" "Microsoft Yahei" "Microsoft_Yahei" "SimHei"
                 "文泉驿微米黑" "文泉驿等宽微米黑" "微软雅黑" "文泉驿等宽正黑" "黑体"
                 "文泉驿正黑" "文泉驿点阵正黑" "华文细黑"))
  "Set fonts candidates which will used by latex."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-latex-commands
  '("xelatex -interaction nonstopmode -output-directory %o %f"
    "bibtex %b"
    "xelatex -interaction nonstopmode -output-directory %o %f"
    "xelatex -interaction nonstopmode -output-directory %o %f")
  "Set latex commands which will be used by `org2ctex-latex-compile'."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-latex-preview-commands
  '("xelatex -interaction nonstopmode -output-directory %o %f")
  "Override `org-latex-preview-commands'.

It will be used by `org2ctex-latex-compile'.

Note: this option *only* useful for ‘org-mode’ (version < 9.0) ."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-preview-latex-process-alist
  '((dvipng
     :programs ("latex" "dvipng" "gs")
     :description "dvi > png"
     :message "you need to install programs: latex, dvipng and ghostscript."
     :image-input-type "dvi"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
    (dvisvgm
     :programs ("latex" "dvisvgm" "gs")
     :description "dvi > svg"
     :message "you need to install programs: latex, dvisvgm and ghostscript."
     :use-xcolor t
     :image-input-type "dvi"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
    (dvisvgm-xelatex
     :programs ("latex" "dvisvgm" "gs")
     :description "dvi > svg"
     :message "you need to install programs: latex, dvisvgm and ghostscript."
     :use-xcolor t
     :image-input-type "xdv"
     :image-output-type "svg"
     :image-size-adjust (1.7 . 1.5)
     :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
     :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick
     :programs ("latex" "convert" "gs")
     :description "pdf > png"
     :message
     "you need to install programs: latex, imagemagick and ghostscript."
     :use-xcolor t
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
     :image-converter
     ("convert -density %D -trim -antialias %f -quality 100 %O")))
  "Override `org-preview-latex-process-alist'.

Please see the info of `org-preview-latex-process-alist',
when `org2ctex-enable' set to t, its value will override
the value of `org-preview-latex-process-alist' before
exporting to latex.

Note: this option is useful for ‘org-mode’ (version >= 9.0 )."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-latex-default-class "ctexart"
  "Default ctex class when export org to LaTeX.

Please see the info of `org-latex-default-class',
when `org2ctex-enable' set to t, its value will override
the value of `org-latex-default-class' before exporting to latex."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-preview-latex-default-process 'dvipng
  "Override `org-preview-latex-default-process'.

Please see the info of `org-preview-latex-default-process',
when `org2ctex-enable' set to t, its value will override
the value of `org-preview-latex-default-process' before
exporting to latex."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-latex-classes
  '(("ctexart"
     "\\documentclass[fontset=none,UTF8,a4paper,zihao=-4]{ctexart}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("ctexrep"
     "\\documentclass[fontset=none,UTF8,a4paper,zihao=-4]{ctexrep}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("ctexbook"
     "\\documentclass[fontset=none,UTF8,a4paper,zihao=-4]{ctexbook}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("beamer"
     "\\documentclass{beamer}
               \\usepackage[fontset=none,UTF8,a4paper,zihao=-4]{ctex}"
     org-beamer-sectioning))
  "Override `org-latex-classes'.

Please see the info of `org-latex-classes',
when `org2ctex-enable' set to t, its value will override
the value of `org2ctex-latex-classes' before exporting to latex."
  :group 'org-export-latex-chinese)

(defcustom org2ctex-latex-default-packages-alist
  (remove '("normalem" "ulem" t)
          (remove '("T1" "fontenc" t)
                  (remove '("AUTO" "inputenc" t)
                          org-latex-default-packages-alist)))
  "Override `org-latex-default-packages-alist'.

Please see the info of `org-latex-default-packages-alist',
when `org2ctex-enable' set to t, its value will override
the value of `org-latex-default-packages-alist' before
exporting to latex.

org 不建议自定义 org-latex-default-package-alist 变量，但
'inputenc' and 'fontenc' 两个宏包似乎和 xelatex 有冲突，调整！"
  :group 'org-export-latex-chinese)

(defcustom  org2ctex-latex-packages-alist
  (list
   "
%%% 默认使用的latex宏包 %%%
\\usepackage{tikz}
\\usepackage{CJKulem}
\\usepackage{graphicx}

%%% 设置页面边距 %%%
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry} %")
  "Override `org-latex-packages-alist'.

Please see the info of `org-latex-packages-alist',
when `org2ctex-enable' set to t, its value will override
the value of `org-latex-packages-alist'
before exporting to latex."
  :group 'org-export-latex-chinese)

;; latex公式预览, 调整latex预览时使用的header,默认使用ctexart类
(defcustom org2ctex-format-latex-header
  (replace-regexp-in-string
   "\\\\documentclass{.*}"
   "\\\\documentclass[nofonts,UTF8]{ctexart}"
   org-format-latex-header)
  "Override `org-format-latex-header'.

Please see the info of `org-format-latex-header',
when `org2ctex-enable' set to t, its value will override
the value of `org-format-latex-header' before
exporting to latex."
  :group 'org-export-latex-chinese)

(defvar org2ctex-enable nil
  "判断是否开启 org2ctex.")

(defconst org2ctex-overrided-variables
  '(org-latex-default-class
    org-latex-classes
    org-latex-packages-alist
    org-format-latex-header
    org-latex-default-packages-alist
    org-preview-latex-process-alist
    org-preview-latex-default-process)
  "记录所有被 org2ctex 包强制覆盖得变量.")

(defun org2ctex-generate-latex-fonts-setting ()
  "Generate a latex fonts setting."
  (when org2ctex-latex-fonts
    (let ((mainfont (org2ctex-get-available-font 'mainfont))
          (cjkmainfont-italic (org2ctex-get-available-font 'CJKmainfont-italic))
          (cjkmainfont (org2ctex-get-available-font 'CJKmainfont))
          (cjksansfont (org2ctex-get-available-font 'CJKsansfont))
          (cjkmonofont (org2ctex-get-available-font 'CJKmonofont)))
      (concat "\n"
              (when mainfont (format "\\setmainfont{%s}\n" mainfont))
              (when cjkmainfont
                (if cjkmainfont-italic
                    (format "\\setCJKmainfont[ItalicFont={%s}]{%s}\n" cjkmainfont-italic cjkmainfont)
                  (format "\\setCJKmainfont{%s}\n" cjkmainfont)))
              (when cjksansfont (format "\\setCJKsansfont{%s}\n" cjksansfont))
              (when cjkmonofont (format "\\setCJKmonofont{%s}\n" cjkmonofont))))))

(defun org2ctex-get-available-font (fontclass)
  "获取可用字体.

Argument FONTCLASS"
  (when org2ctex-latex-fonts
    (let* ((fonts-list (cdr (assoc fontclass org2ctex-latex-fonts)))
           (font (car (cl-remove-if
                       #'(lambda (fontname)
                           (not (org2ctex-font-available-p fontname)))
                       fonts-list))))
      (unless font
        (message (format "org-latex-chinese: Emacs can't find an available (%s) font for latex, ignore!"
                         (symbol-name fontclass))))
      font)))

(defun org2ctex-font-available-p (fontname)
  "测试 FONTNAME 是否是一个可用字体."
  (mapcar #'(lambda (x)
              (substring-no-properties x))
          (delq nil (mapcar
                     #'(lambda (x)
                         (when (or (string= fontname x)
                                   (string= (string-as-unibyte fontname) x))
                           fontname))
                     (font-family-list)))))

(defun org2ctex-insert-setq-sexp (variable &optional value)
  "在当前 buffer 中插入一个 setq 表达式.

这个表达式中，变量位置插入 VARIABLE, 取值位置插入 VALUE."
  (let ((value (or value
                   (symbol-value
                    (intern (replace-regexp-in-string
                             "^org-" "org2ctex-" (symbol-name variable)))))))
    (insert
     (cond ((stringp value) (format "(setq %S %S)\n" variable value))
           ((symbolp value) (format "(setq %S '%S)\n" variable value))
           ((listp value)
            (format "(setq %S\n (quote %s))\n"
                    variable
                    (replace-regexp-in-string
                     "\n\n" "\n"
                     (with-temp-buffer
                       (emacs-lisp-mode)
                       (goto-char (point-min))
                       (cl-prettyprint value)
                       (buffer-string)))))
           (t (format "(setq %S %S)\n" variable value))))))

(defun org2ctex-insert-configure-template ()
  "依据 org2ctex 的设置，在当前 buffer 中插入一个对应的 org latex 配置."
  (interactive)
  (when (yes-or-no-p "是否在光标处插入一个 org 中文配置模板（根据 org2ctex 的设置生成）？ ")
    (insert (with-temp-buffer
              (insert (concat
                       ";; Auto generated by `org2ctex-insert-configure-template' of\n"
                       ";; (org2ctex)[https://github.com/tumashu/org2ctex]\n"))
              (dolist (x '(org-latex-default-class
                           org-latex-classes
                           org-latex-default-packages-alist))
                (org2ctex-insert-setq-sexp x))
              (org2ctex-insert-setq-sexp 'org-latex-pdf-process
                                         org2ctex-latex-commands)
              (org2ctex-insert-setq-sexp 'org-latex-packages-alist
                                         (if (org2ctex-generate-latex-fonts-setting)
                                             (cons (org2ctex-generate-latex-fonts-setting)
                                                   org2ctex-latex-packages-alist)
                                           org2ctex-latex-packages-alist))
              (unless (string< (org-version) "9.0")
                (org2ctex-insert-setq-sexp 'org-preview-latex-default-process)
                (org2ctex-insert-setq-sexp 'org-preview-latex-process-alist))
              (insert ";; The end of auto generated code.")
              (buffer-string)))))

(defmacro org2ctex-if (cond expression1 expression2)
  "如果 COND 为 t, 在一个 org2ctex 环境中执行 EXPRESSION1 表达式，否则执行 EXPRESSION2."
  (declare (indent 1))
  `(if ,cond
       (let ((org-latex-default-class org2ctex-latex-default-class)
             (org-latex-classes org2ctex-latex-classes)
             (org-latex-default-packages-alist org2ctex-latex-default-packages-alist)
             (org-latex-packages-alist
              (if (org2ctex-generate-latex-fonts-setting)
                  (cons (org2ctex-generate-latex-fonts-setting)
                        org2ctex-latex-packages-alist)
                org2ctex-latex-packages-alist))
             (org-latex-pdf-process org2ctex-latex-commands)
             (org-preview-latex-default-process org2ctex-preview-latex-default-process)
             (org-preview-latex-process-alist org2ctex-preview-latex-process-alist))
         ,expression1)
     ,expression2))

(defun org2ctex-export-as (orig-fun backend &optional subtreep
                                    visible-only body-only ext-plist)
  "Org2ctex 化的 `org-export-as, 做为 advice 函数使用.

Argument ORIG-FUN, BACKEND,
Optional argument SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST."
  (org2ctex-if (and org2ctex-enable
                    (member backend '(latex beamer)))
               (funcall orig-fun backend subtreep visible-only body-only ext-plist)
               (funcall orig-fun backend subtreep visible-only body-only ext-plist)))

(defun org2ctex-create-formula-image-with-imagemagick (orig-fun string tofile options buffer)
  "Org2ctex 化的 `org-create-formula-image-with-imagemagick', advice 函数.

Argument: ORIG-FUN STRING TOFILE OPTIONS BUFFER."
  (org2ctex-if org2ctex-enable
               (funcall orig-fun string tofile options buffer)
               (funcall orig-fun string tofile options buffer)))

(defun org2ctex-latex-compile (orig-fun texfile &optional snippet)
  "Org2ctex 化的 `org-latex-compile', advice 函数.

Argument: ORIG-FUN TEXFILE SNIPPET."
  (org2ctex-if org2ctex-enable
               (let ((org-latex-pdf-process
                      (if snippet
                          org2ctex-latex-preview-commands
                        org2ctex-latex-commands)))
                 (funcall orig-fun texfile snippet))
               (funcall orig-fun texfile snippet)))

(defun org2ctex-toggle (&optional force-enable)
  "启用/禁用 org2ctex 包.

如果 FORCE-ENABLE is t, 强制启用 org2ctex."
  (interactive)
  (setq org2ctex-enable
        (or force-enable (not org2ctex-enable)))
  (if org2ctex-enable
      (progn
        (message (concat "Org2ctex is enabled, force *override*："
                         (mapconcat #'symbol-name org2ctex-overrided-variables ", ")
                         "."))
        (advice-add 'org-export-as :around #'org2ctex-export-as)
        (advice-add 'org-create-formula-image-with-imagemagick
                    :around #'org2ctex-create-formula-image-with-imagemagick)
        (advice-add 'org-latex-compile :around #'org2ctex-latex-compile))
    (message "Org2ctex is disabled.")
    (advice-remove 'org-export-as #'org2ctex-export-as)
    (advice-remove 'org-create-formula-image-with-imagemagick
                   #'org2ctex-create-formula-image-with-imagemagick)
    (advice-remove 'org-latex-compile #'org2ctex-latex-compile)))

(provide 'org2ctex)

;; * Footer

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; org2ctex.el ends here
