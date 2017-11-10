fork of `linum-mode'
Overlay scrollbar on line number.

linum.elをベースとして、
現在の位置がバッファ全体から見てどのぐらいの位置かをスクロールバーのように表示する機能を追加したもの。
主に変更したのは yalinum-update-window。
Installation:
(require 'yalinum)
(global-yalinum-mode t)
