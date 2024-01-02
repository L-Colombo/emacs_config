(unless (file-exists-p "~/.emacs.d/custom.el")
  (shell-command "touch ~/.emacs.d/custom.el"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(org-babel-load-file "~/.emacs.d/configuration.org")
