(when (maybe-require-package 'docker)
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))
(maybe-require-package 'dockerfile-mode)
(maybe-require-package 'docker-compose-mode)


(provide 'init-docker)
