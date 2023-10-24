(auto-save-mode -1)
(setq inhibit-startup-screen 't)
(setq-default word-wrap t)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'default nil :height 190)
(setq make-backup-files nil)
(setq auto-save-default nil)
(electric-pair-mode 1)
(windmove-default-keybindings)


;; meta is now command on mac
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)


;; Installing MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load Colorscheme
(load-theme 'ef-bio t)

;; Auctex
(require 'package)
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(require 'tex)
(setq TeX-parse-self t)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;;REFTEX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;;Ido-mode
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Org mode
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'content)
(require 'org-bullets) ;;Org-bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-truncated nil)

;; auto-complete
(use-package auto-complete
  :ensure t
  :init (progn
	  (ac-config-default)
	  (global-auto-complete-mode t)))

;; Eglot-LSP
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

;; Snippets
(yas-global-mode 1)
(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'javascript-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'c++-mode-hook 'add-yasnippet-ac-sources)

;;Vertico
(use-package vertico
  :init
  (vertico-mode))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("df2cdf4ffb933c929b6a95d60ac375013335b61565b9ebf02177b86e5e4d643f" "9a456f2aac10f18204e8ece27c84950c359f91bb06bda8c711bf4f5095ca8250" "13bf32d92677469e577baa02d654d462f074c115597652c1a3fce6872502bbea" "ddb9bc949afc4ead71a8861e68ad364cd3c512890be51e23a34e4ba5a18b0ade" "a8a5fd1c8afea56c5943ead67442a652f1f64c8191c257d76988edb0b1ad5dfa" "a876039e0832c9a0e11af80ffbdbb4539aede1fbdc19460290fc4d1bf3a21741" "86a9bfbda652a2dd887077a4ad91afbec2fde569e26147ceb8a605976c99d8d2" "b940c68944436ab216d83c13a05808bcacf40ac224c4aba2c209c3cbf2c76427" "4d4475c85408bbc9d71e692dd05d55c6b753d64847f5e364d1ebec78d43e2aef" "b0dc32efddfd36f0a12d022ac3c79a3d6d9614558bc8a991e5a5a29be70dafe9" "917d7e7f0483dc90a5e2791db980ce9bc39e109a29198c6e9bdcd3d88a200c33" "64c4ff0a617e6bf33443821525f7feb3ef925a939c4575e77f3811c5b32e72c0" "779aa194815bd4f88b672856961077bc3c735cb82d05b440e981bd218749cf18" "6c655326d9bb38d4be02d364d344bfa61b3c8fdabd1cf4b97dddc8c0b3047b47" "2949f71b19f52bcee693534b6b6ad8796e495eb0c676e9c94f3e33f10511eb47" "5014b68d3880d21a5539af6ef40c1e257e9045d224efb3b65bf0ae7ff2a5e17a" "3ca84532551daa1b492545bbfa47fd1b726ca951d8be29c60a3214ced30b86f5" "76c646974f43b321a8fd460a0f5759f916654575da5927d3fd4195029c158018" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "c92a0fece9ad256d83a0ce85df1f15e1c9280eba91c0cc06f8879b9572a855c7" "319c0d5bea1d32bbcf0bfb9acb3ed8dbb1e1afc0feec81ae0b2f20ce7d7104b4" "76eb683894aa00c77e55f482a23233d0007d59cc47a5653a37ad2897f31330b0" "18d1131ad6ff8e8e34287d6de9299faba4f1b03874278463fb6f38e3abe65bdc" "d3a63c40fddcd2369b7465beca48ddd61fa8e980e9fa88adc52c3cfc78b2b352" "3b69ddebc3b1386c63e70afa0eca90d1a775c52ad144d16df932400f3afe1c30" "41f9d9c23510f590bc76f4ac16987b013a65847059081d405f31e5e828475f3b" "9cda05ef581a03ce47f9e490c241f659520be02a7318757048db32fef4421da9" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" "5c64430cb8e12e2486cd9f74d4ce5172e00f8e633095d27edd212787a4225245" "6950cff91f52578d46e0c3c0b68d329a72157cca1e2380e2e8e7557b8257eb6d" "60c23c3a831c9f28b10084e8642b7d987d33be3faee8f68d68d1cf6b171041da" default))
 '(package-selected-packages
   '(ido-vertical-mode vertico yasnippet-snippets yasnippet markdown-mode auto-complete ef-themes magit paganini-theme cyberpunk-theme pdf-tools org-bullets gruber-darker-theme auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
