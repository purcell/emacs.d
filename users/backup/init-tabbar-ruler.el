;; https://github.com/mlf176f2/tabbar-ruler.el

(setq tabbar-ruler-global-tabbar t)     ; If you want tabbar
(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
(setq tabbar-ruler-popup-menu nil) ; If you want a popup menu.
(setq tabbar-ruler-popup-toolbar nil) ; If you want a popup toolbar
(setq tabbar-ruler-popup-scrollbar nil) ; If you want to only show the
(require 'tabbar-ruler)

;scroll bar when your mouse is moving.
;;(tabbar-ruler-group-buffer-groups)
(tabbar-ruler-group-by-projectile-project)


(provide 'init-tabbar-ruler)
;;; init-tabbar-ruler.el ends here
