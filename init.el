;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DEFAULT SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(auto-save-mode -1)
(setq inhibit-startup-screen 't)
(setq-default word-wrap t)
(tool-bar-mode 1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(set-frame-font "Iosevka Nerd Font Propo 20" nil t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(electric-pair-mode 1)

;;move across window with
(windmove-mode -1)
(global-set-key (kbd "C-c <right>") #'windmove-right)
(global-set-key (kbd "C-c <left>") #'windmove-left)
(global-set-key (kbd "C-c <up>") #'windmove-up)
(global-set-key (kbd "C-c <down>") #'windmove-down)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PACKAGES AND REPO INITIALIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HELPFUL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'helpful)
  (package-install 'helpful))

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'magit)
  (package-install 'magit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;COLORSCHEMES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'naga-theme)
  (package-install 'naga-theme))
(unless (package-installed-p 'gruber-darker-theme)
  (package-install 'gruber-darker-theme))

(load-theme 'gruber-darker t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PDF-TOOLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(setq pdf-view-incompatible-modes '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;AUCTEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'auctex)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;IDO & VERTICO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'ido-vertical-mode)
  (package-install 'ido-vertical-mode))

(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package vertico
  :init
  (vertico-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ORG-MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'content)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-truncated nil)

;;setup agenda
(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-agenda-files '("~/.orgmode_files/Agenda.org"))
(setq org-agenda-span 'month)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;AUTO-COMPLETE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(use-package auto-complete
  :ensure t
  :init (progn
	  (ac-config-default)
	  (global-auto-complete-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EGLOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'eglot)
  (package-install 'eglot))

(setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "/usr/local/bin/intelephense" "--stdio")))
(add-hook 'php-mode-hook 'eglot-ensure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;YASNIPPET & SNIPPETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DASHBOARD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-items '((bookmarks . 7)
			(recents . 7)
			(projects . 7)))

(setq dashboard-icon-type 'all-the-icons)
(setq dashboard-set-heading-icons nil)
(setq dashboard-set-file-icons t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MOVE-TEXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MODE-LINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))

(require 'doom-modeline)
(doom-modeline-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NEO-TREE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'neotree)
  (package-install 'neotree))

(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
;;to work properly, run `M-x all-the-icons-install-fonts RET`

(require 'neotree)
(global-set-key (kbd "C-c e") 'neotree-toggle)
(setq neo-smart-open t)

(when (display-graphic-p)
  (require 'all-the-icons))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;LANGUAGE-SPECIFIC MODES;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'php-mode)
  (package-install 'php-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))
