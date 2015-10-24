;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *系统存在* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。
(setq cfs--custom-set-fontnames
      '(
        ("monofur" "Consolas" "DejaVu Sans Mono" "Droid Sans Mono" "PragmataPro" "Courier" "Courier New" "Liberation Mono" "Ubuntu Mono" "Droid Sans Mono Pro" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
        ("微软雅黑" "Microsoft Yahei" "Microsoft_Yahei" "文泉驿等宽微米黑" "文泉驿等宽正黑" "黑体" "Hiragino Sans GB" "文泉驿正黑" "文泉驿点阵正黑" "SimHei" "SimSun" "NSimSun" "FangSong" "KaiTi" "FangSong_GB2312" "KaiTi_GB2312" "LiSu" "YouYuan" "新宋体" "宋体" "楷体_GB2312" "仿宋_GB2312" "幼圆" "隶书" "STXihei" "STKaiti" "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti" "STXingkai" "STXinwei" "方正姚体" "方正舒体" "方正粗圆_GBK" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文细黑" "华文行楷")
        ("HanaMinB")
        ))

;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
(setq cfs--custom-set-fontsizes
      '(
        (9    10.5 10.5)
        (12   12.5 12.5)
        (13   14.0 15.0)
        (14.0   14.5  15.0)
        (15.0   15.5 15.0)
        (16.0   16.5 16.5)
        (17.0   18.0 18.0)
        (18.0   18.5 18.5)
        (19.0   20.0 20.0)
        (20.0   21.0 21.0)
        (23   24.0 24.0)
        (25   26.0 26.0)
        ))
