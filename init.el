(auto-save-mode -1)
(setq inhibit-startup-screen 't)
(setq-default word-wrap t)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'default nil :height 190)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
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
(load-theme 'naga t)

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

(setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1)))) ;; disable eldoc info by default

(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(php-mode "~/.nvm/versions/node/v18.16.1/bin/intelephense" "--stdio")))
(add-hook 'php-mode-hook 'eglot-ensure)

;; Snippets
(yas-global-mode 1)
(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'javascript-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'c++-mode-hook 'add-yasnippet-ac-sources)
(add-hook 'php-mode-hook 'add-yasnippet-ac-sources)

;;Vertico
(use-package vertico
  :init
  (vertico-mode))

;;Dashboard

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents . 5)
			(projects . 5)
			))

;;Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f84dbe5cfa80aa6774c57fef30d76bcdeb71bd0077665fb74f75728c42f5675d" "ed6e47baf355da248c4de8953058234b82c8f838fc85f570f9fe1700e47b9426" "64204b9e3ad01000654d5524d2904fc8fa28aafc168f48660897ddfe36a2bfd5" "454e92bc5f22f690afce91cb6f92a3ccb638c57a89e84c55759fb16dfb2444e4" "efd849c804148b88536914ccdee08285fd7376e2e3334522c9afc00fd7e594da" "179936f522293a0963e57e17b1e8e09615dfb82cffebe32950f49c7cccb2fea6" "9f3c0f73a108b2fb169dd487c57b524153e40946b85a4612d30320b2971b6d22" "4ba6aa8a2776688ef7fbf3eb2b5addfd86d6e8516a701e69720b705d0fbe7f08" "46586a9fe1e34359c55c43536aa362c15237a29dbbd69a225ddcbcce97301959" "5a45c8bf60607dfa077b3e23edfb8df0f37c4759356682adf7ab762ba6b10600" "bdb4509c123230a059d89fc837c40defdecee8279c741b7f060196b343b2d18d" "ed17fef69db375ae1ced71fdc12e543448827aac5eb7166d2fd05f4c95a7be71" "f6fe76b3b55c0b493e0411459a98534c584feaa47ab8427a2433492248cd1643" "9b39b25c3a23b1be6e99a3648b91ebaf2a7efdde236e3472aa95f1708ec61d4f" "5283a0c77cc7640fc28493cfdf8957b11e1c72af846d96f5e5a6a37432264c34" "73b6fb50100174334d220498186ab5ca3ade90052f5a08e8262e5d7820f0a149" "46325e20421d2b4423cc90db5a35e0a45bd78a6f3e26c52314b189af3cc00733" "59a6480c96f8af4bb65610c4efcee189e1a80c2f31407584dd62887664bb541b" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "043d7daffa6eab75097150fa0c692c2509a2ecb0a07997c7c2ed577f1fded39c" "e8ab68ce371f48623dab9658d7910458e98affec3e09585a39552dbd3fd1ecda" "3454885b915a176dce4b53e35053b7ee0aa9362fb9e934057ac44b6842a97453" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "70e7f094987e3c6226c54078dd986e11cab7246ea1c9e58a9907afa90f3c10c9" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "162201cf5b5899938cfaec99c8cb35a2f1bf0775fc9ccbf5e63130a1ea217213" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "6adeb971e4d5fe32bee0d5b1302bc0dfd70d4b42bad61e1c346599a6dc9569b5" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "0d0936adf23bba16569c73876991168d0aed969d1e095c3f68d61f87dd7bab9a" "9dccdccfeb236623d5c7cf0250a92308cf307afde4ebdaf173b59e8bbbae1828" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "a9dc7790550dcdb88a23d9f81cc0333490529a20e160a8599a6ceaf1104192b5" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "29b4f767c48da68f8f3c2bbf0dde2be58e4ed9c97e685af5a7ab7844f0d08b8b" "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "65a1a112abd99456167df57ce2cfff42ed137c4f9146e75b2ae9812499689c3a" "6c01b5d4faa0f143055e63c9fba8e23e9160f181e54b14b46d56410811edbc9e" "6ed8a3705a4296955010ecfcf808f02ac0d52985373e07c63f7fe5bc85206bb4" "9b64a681308383067359cf06bfa6a1bc4fa75c5b68182e4d6ba4d1816277d70e" "d9717212622f16f6b9e0bccc99f98761cbeb14065c4d9fa7d88f6b4507a0dbf6" "0e47dae5cf4a4c4c2050380c76d3e3b5184e760ea97f357b8e64a6dd76d863bd" "f87f74ecd2ff6dc433fb4af4e76d19342ea4c50e4cd6c265b712083609c9b567" "438f0e2b9fd637c53b20c27c140d2fc14fa154acf9ef92630666cab497c69742" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "df2cdf4ffb933c929b6a95d60ac375013335b61565b9ebf02177b86e5e4d643f" "9a456f2aac10f18204e8ece27c84950c359f91bb06bda8c711bf4f5095ca8250" "13bf32d92677469e577baa02d654d462f074c115597652c1a3fce6872502bbea" "ddb9bc949afc4ead71a8861e68ad364cd3c512890be51e23a34e4ba5a18b0ade" "a8a5fd1c8afea56c5943ead67442a652f1f64c8191c257d76988edb0b1ad5dfa" "a876039e0832c9a0e11af80ffbdbb4539aede1fbdc19460290fc4d1bf3a21741" "86a9bfbda652a2dd887077a4ad91afbec2fde569e26147ceb8a605976c99d8d2" "b940c68944436ab216d83c13a05808bcacf40ac224c4aba2c209c3cbf2c76427" "4d4475c85408bbc9d71e692dd05d55c6b753d64847f5e364d1ebec78d43e2aef" "b0dc32efddfd36f0a12d022ac3c79a3d6d9614558bc8a991e5a5a29be70dafe9" "917d7e7f0483dc90a5e2791db980ce9bc39e109a29198c6e9bdcd3d88a200c33" "64c4ff0a617e6bf33443821525f7feb3ef925a939c4575e77f3811c5b32e72c0" "779aa194815bd4f88b672856961077bc3c735cb82d05b440e981bd218749cf18" "6c655326d9bb38d4be02d364d344bfa61b3c8fdabd1cf4b97dddc8c0b3047b47" "2949f71b19f52bcee693534b6b6ad8796e495eb0c676e9c94f3e33f10511eb47" "5014b68d3880d21a5539af6ef40c1e257e9045d224efb3b65bf0ae7ff2a5e17a" "3ca84532551daa1b492545bbfa47fd1b726ca951d8be29c60a3214ced30b86f5" "76c646974f43b321a8fd460a0f5759f916654575da5927d3fd4195029c158018" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "c92a0fece9ad256d83a0ce85df1f15e1c9280eba91c0cc06f8879b9572a855c7" "319c0d5bea1d32bbcf0bfb9acb3ed8dbb1e1afc0feec81ae0b2f20ce7d7104b4" "76eb683894aa00c77e55f482a23233d0007d59cc47a5653a37ad2897f31330b0" "18d1131ad6ff8e8e34287d6de9299faba4f1b03874278463fb6f38e3abe65bdc" "d3a63c40fddcd2369b7465beca48ddd61fa8e980e9fa88adc52c3cfc78b2b352" "3b69ddebc3b1386c63e70afa0eca90d1a775c52ad144d16df932400f3afe1c30" "41f9d9c23510f590bc76f4ac16987b013a65847059081d405f31e5e828475f3b" "9cda05ef581a03ce47f9e490c241f659520be02a7318757048db32fef4421da9" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" "5c64430cb8e12e2486cd9f74d4ce5172e00f8e633095d27edd212787a4225245" "6950cff91f52578d46e0c3c0b68d329a72157cca1e2380e2e8e7557b8257eb6d" "60c23c3a831c9f28b10084e8642b7d987d33be3faee8f68d68d1cf6b171041da" default))
 '(package-selected-packages
   '(projectile dashboard calmer-forest-theme gotham-theme abgaben naga-theme kaolin-themes oblivion-theme rust-mode php-mode ido-vertical-mode vertico yasnippet-snippets yasnippet markdown-mode auto-complete ef-themes magit cyberpunk-theme pdf-tools org-bullets gruber-darker-theme auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
