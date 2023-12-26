(auto-save-mode -1)
(setq inhibit-startup-screen 't)
(setq-default word-wrap t)
(tool-bar-mode 1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'default nil :height 190)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(electric-pair-mode 1)
(windmove-default-keybindings)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; CMD af meta key on MacOS
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)


;; Installing MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Colorschemes 
(unless (package-installed-p 'naga-theme) ;;ensures current theme is installed before loading it
  (package-install 'naga-theme))

(load-theme 'naga t)

;; Auctex
(require 'package)

(unless (package-installed-p 'pdf-tools) ;;ensure pdf-tools is installed and configured
  (package-install 'pdf-tools))
(use-package pdf-tools
  :config
  (pdf-tools-install))

(unless (package-installed-p 'auctex) ;;AuCTeX setup
  (package-install 'auctex))
(require 'latex)
(setq TeX-parse-self t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(with-eval-after-load 'tex
  (setq TeX-source-correlate-method '((pdf . synctex)))
  (TeX-source-correlate-mode))

(eval-after-load "tex"
  '(define-key TeX-source-correlate-map [C-down-mouse-1]
	       #'TeX-view-mouse))
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;;Ido-mode
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Org mode
(unless (package-install 'org-bullets)
  (package-install 'org-bullets))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'content)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-truncated nil)

;; auto-completetion
(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(use-package auto-complete
  :ensure t
  :init (progn
	  (ac-config-default)
	  (global-auto-complete-mode t)))

;; Eglot-LSP
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

(setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1)))) ;; disable eldoc info by default
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "~/.nvm/versions/node/v18.16.1/bin/intelephense" "--stdio")))
(add-hook 'php-mode-hook 'eglot-ensure)

;; Snippets
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(unless (package-installed-p 'yasnippet-snippets)
  (package-install 'yasnippet-snippets))

(yas-global-mode 1)
(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'javascript-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'c++-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'php-mode-hook 'add-yasnippet-ac-sources)

;;Vertico
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(use-package vertico
  :init
  (vertico-mode))

;;Dashboard
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents . 5)
			(projects . 5)))

;;Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;Move Text
(unless (package-installed-p 'move-text)
  (package-install 'move-text))

(move-text-default-bindings)
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))
(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)
