;;; notes
;; Guiding principles: Reproducability and portability
;; Please install git, vim, zip, etc
;; For keepass, install keepass cli or kpcli or whatever it's called
;; vterm - cmake, libtool-bin, libvterm
;; For spotify add spotify client and id to ~/.emacs-secrets.el. Also pin spotify to browser/always have it open. Numbers are online
;; For spellcheck, ispell
;; For email follow instructions at https://medium.com/@enzuru/emacs-26-wanderlust-and-modern-gmail-authentication-36e1ae61471f
;; Also, remember you can M-x elmo-passwd-alist-save
;; For pdf editing follow https://github.com/politza/pdf-tools. Comment out pdf-loader-install if necessary
;; For better projectile search? and dumbjump install ripgrep
;; For python development install python(package manager), jedi, black, autopep8, yapf, pyreadline, ipython(pip), flake8, rope
;; For c/c++/obj-c/etc install llvm, irony-server, bear
;; For taking notes/drawing the program "drawing"
;; For latex preview dvipng and 'latex'. For export also install zip
;; Enable debug messages for problems with this file
;; (setq debug-on-error t)
;;; ---------------------------setup package manager-------------------------
;; Set up package.el to work with MELPA
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


;;; --------------------------vim-migration/general-editing------------------------
(setq-default tab-width 4)
(setq-default tab-stop-list 4)
;; Autodetect indentation if possible

(unless (package-installed-p 'dtrt-indent)
  (package-install 'dtrt-indent))
(dtrt-indent-global-mode 1)
;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
;; allow c-u scrolling in evil
(setq evil-want-C-u-scroll t)
;; Enable Evil
;; required for evil-collection
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(require 'evil)
(evil-mode 1)
;; Enable evil collection
(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(evil-collection-init)
(setq evil-search-module 'evil-search)
;; Disable evil unwanted org indentation
(setq evil-auto-indent nil)
;; Autosave backup directory
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
;; Autorefresh files
(global-auto-revert-mode)
;; set leader key in normal state
;; regular undo tree
(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode)
(undo-tree-mode 1)
;; Persistent undo
(setq undo-tree-auto-save-history 1)
;; undo
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
;; redo
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
;; Highlight matching ({[
(show-paren-mode 1)


;; Relative numbering
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(setq display-line-numbers 'visual)
(setq display-line-numbers-type 'relative)

;; which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)
;; snippets
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(require 'yasnippet)
(yas-global-mode 1)
;; Multiple cursors
(unless (package-installed-p 'evil-multiedit)
  (package-install 'evil-multiedit))
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)
;; Symbol list
(unless (package-installed-p 'imenu-list)
  (package-install 'imenu-list))
 
(setq imenu-list-auto-resize t)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(setq imenu-list-focus-after-activation t)
;; Dumb Jump

(unless (package-installed-p 'dumb-jump)
  (package-install 'dumb-jump))
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; Warn when lines are too long
(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
;; Edit in firefox
(unless (package-installed-p 'atomic-chrome)
  (package-install 'atomic-chrome))
(require 'atomic-chrome)
(atomic-chrome-start-server)
;;; ------------------------navigation---------------------------

;; Sentences don't need 2 spaces after.
(setf sentence-end-double-space nil)

;; Projectile project jumping

(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)
;; See https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile
(setq projectile-enable-caching t)
;; Go to next and previous buffer
(global-set-key (kbd "C-{") 'previous-buffer)
(global-set-key (kbd "C-}") 'next-buffer)
;; Jump to new split when creating it
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(define-key evil-normal-state-map (kbd "C-w C-v") 'split-and-follow-vertically)
(define-key evil-normal-state-map (kbd "C-w C-s") 'split-and-follow-horizontally)
(define-key evil-normal-state-map (kbd "C-w v") 'split-and-follow-vertically)
(define-key evil-normal-state-map (kbd "C-w s") 'split-and-follow-horizontally)
;; Allow C-w C-hjkl to jump windows
(define-key evil-normal-state-map (kbd "C-w C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-w C-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-w C-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "C-w C-l") 'windmove-right)
;; Go to other window
(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))
(global-set-key (kbd "M-o") 'ace-window)
;; Use letter keys for window switch
(setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i))
;; Jump to largest window

;; Jump to largest window
(defun jump-to-largest-window ()
  "Move to the largest window"
  (interactive)
  ; sort windows and select largest
  (select-window (nth 0 (sort (window-list)
		(lambda (a b) (> (* (window-total-height a) (window-total-width a))
							(* (window-total-height b) (window-total-width b))))))))
		

;; Smart splitting. Sorta like bpswm
(defun split-along-longer-side ()
  "Jump to largest window and split along its longest side."
  (interactive)
  (jump-to-largest-window)
  (if (> (window-pixel-width) (window-pixel-height))
	  (split-and-follow-vertically)
	(split-and-follow-horizontally)))
(global-set-key (kbd "C-c <C-return>") 'split-along-longer-side)
;; Window resizeing
(define-key evil-normal-state-map (kbd "+") 'enlarge-window)
(define-key evil-normal-state-map (kbd "-") 'shrink-window)

; fuzzy find ido
(unless (package-installed-p 'flx-ido)
  (package-install 'flx-ido))
; Enable ido
(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

; ido vertical and c-n c-p
(unless (package-installed-p 'ido-vertical-mode)
  (package-install 'ido-vertical-mode))
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
  
;;; ---------------------------aesthetics-------------------------
;; Disable toolbar/menubar/:q
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Theme
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))
(load-theme 'solarized-dark t)

;;; ------------------------------org-mode-------------------------
;; Spellcheck
(add-hook 'org-mode-hook 'flyspell-mode)
;; pretty bullets :)
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-with-inline-images t) ; show inline images
;; Start drawing software and insert new drawing into current line
(defun create-img ()
  "Prompt for a filename, then call up drawing to create an image. Add to org mode doc."
  (interactive)
  (let ((imgfile (read-string "Filename? " "" 'my-history)))
	(end-of-line)
	(if (not (file-exists-p (concat "./" imgfile)))
      (insert "\n[[./" imgfile "]]\n" ))
    (start-process-shell-command "drawing" nil "drawing")
  ))
(global-set-key (kbd "C-c d") 'create-img)
;; add finished times to org
(setq org-log-done 'time)
;; org indentation
(setq org-startup-indented t)
;; abbreviate TODO and DONE
(setq org-todo-keywords
      '((sequence "LATER(l)" "TODO(t)" "URGENT(u)" "|" "DONE(d)")))
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list (concat org-directory "lists.org"))) ; Only look at todo list
(setq org-agenda-start-on-weekday nil) ; Make agenda start on current day
(setq org-todo-keyword-faces
      '(("URGENT" . org-warning)))
;; Open agenda
(global-set-key (kbd "C-c a" ) 'org-agenda)
;; Bindings to common files
(global-set-key (kbd "C-c f o" )
				(lambda () (interactive)
				  (find-file (concat org-directory "lists.org"))))
(global-set-key (kbd "C-c f e" )
				(lambda () (interactive)
				  (find-file "~/.config/nixpkgs/emacs/init.el")))
(global-set-key (kbd "C-c f h" )
				(lambda () (interactive)
				  (find-file "~/.config/nixpkgs/home.nix")))
(global-set-key (kbd "C-c f c" )
				(lambda () (interactive)
				  (find-file (concat org-directory "~/.config/nixpkgs/nixos/configuration.nix"))))
;; Snippets
(setq org-agenda-restore-windows-after-quit t)
(setq org-default-notes-file (concat org-directory "lists.org"))
(global-set-key (kbd "C-c c") 'org-capture)
;; Required to get org and yas working.
;; see https://orgmode.org/manual/Conflicts.html#Conflicts
(add-hook 'org-mode-hook
		  (lambda ()
			(setq-local yas/trigger-key [tab])
			(define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
		  (lambda ()
			(make-variable-buffer-local 'yas/trigger-key)
			(setq yas/trigger-key [tab])
			(add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
			(define-key yas/keymap [tab] 'yas/next-field)))

(eval-after-load 'yasnippet
'(yas-define-snippets 'org-mode
					 '(("bmatrix" "\\begin{bmatrix}\n$1\n\\end{bmatrix}" "Insert a latex bmatrix")
					   ("bmx" "\\begin{bmatrix}$1\\end{bmatrix}$2" "Insert a latex bmatrix")
					   ("pd" "\\frac{\\partial $1}{\\partial $2}" "Insert a latex partial derivative")
					   ("\$pd" "\$\\frac{\\partial $1}{\\partial $2}" "Insert a latex pd part 2")
					   ("$bmx" "\$\\begin{bmatrix}$1\\end{bmatrix}$2\$" "Insert a latex bmatrix pt 2")
					   ("begin" "\\begin{$1}\n$2\n\\end{$1}" "Begin stuff")
					   ("gather" "\\begin{gather*}\n$1\n\\end{gather*}" "Begin stuff")
					   ("frc" "\\frac{$1}\\{$2}" "Insert a latex fraction")
					   ("box" "\\begin{tcolorbox}$1\\end{tcolorbox}" "tcolorbox - latex")
					   ("*" "\\cdot" "dot product")
					   ("fourier" "\\frac{v \\cdot v_i}{v_i \\cdot v_i} * v_i" "Fourier formula")
					   ("equation" "\\begin{equation*}\n\\begin{align*}\n$1\n\\end{align*}\n\\end{equation*}\n" "Insert latex code"))))
;;; ---------------------spotify----------------------
;; Settings
(when (file-exists-p "~/.emacs-secrets.el")
  (unless (package-installed-p 'simple-httpd)
	(package-install 'simple-httpd))
  (load "~/.emacs-secrets")
  (when (not (file-exists-p "~/.emacs.d/spotify"))
	  (shell-command "git clone https://github.com/danielfm/spotify.el ~/.emacs.d/spotify"))
  (add-to-list 'load-path "~/.emacs.d/spotify")
  (require 'spotify)
  (setq spotify-oauth2-client-secret spotify-secret)
  (setq spotify-oauth2-client-id spotify-id)
  (setq spotify-transport 'connect)
  (global-set-key (kbd "C-j" ) 'spotify-volume-down)
  (global-set-key (kbd "C-k" ) 'spotify-volume-up)
  (global-set-key (kbd "M-p M-d" ) 'spotify-select-device)
  (global-set-key (kbd "C-M-;" ) 'spotify-toggle-play)
  (global-set-key (kbd "M-p M-n" ) 'spotify-next-track)
  (global-set-key (kbd "M-p M-p" ) 'spotify-previous-track))

;;; ----------------------pdf--------------------------
;; If pdftools isn't installed it uses doc-view mode
(evil-define-key 'normal doc-view-minor-mode-map
  "h" 'doc-view-previous-page
  "j" (lambda () (interactive) (doc-view-next-line-or-next-page 5))
  "k" (lambda () (interactive) (doc-view-previous-line-or-previous-page 5))
  "l" 'doc-view-next-page)
(unless (package-installed-p 'let-alist)
  (package-install 'let-alist))
(unless (package-installed-p 'tablist)
  (package-install 'tablist))
(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))
(pdf-loader-install)

;;; --------------------------------magit--------------------------
(unless (package-installed-p 'magit)
  (package-install 'magit))

;;; ----------------------------dired------------------------
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.trash")

;;; --------------------------eww--------------------
(defun eww-new ()
  "Open eww in new buffer"
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer (concat "eww - " url)))
    (eww-mode)
    (eww url)))
(global-set-key (kbd "C-c s b" ) 'eww)
(global-set-key (kbd "C-c s B" ) 'eww-new)
(evil-collection-define-key 'normal 'eww-mode-map "O" 'eww-new)
(setq eww-search-prefix "https://www.google.com/search?q=")

;;; --------------------mail------------------------
(global-set-key (kbd "C-c s m" ) 'wl)
(unless (package-installed-p 'wanderlust)
  (package-install 'wanderlust))
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "dghosef@gmail.com") 
(setq elmo-imap4-default-authenticate-type 'clear) 
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "dghosef")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t) 

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(setq wl-from "Joseph Tan <dghosef@gmail.com>")
;;; keepass
(unless (package-installed-p 'keepass-mode)
  (package-install 'keepass-mode))
(require 'keepass-mode)
(define-key keepass-mode-map (kbd "C-c C-c") 'keepass-mode-copy-password)
(defun open-passwords ()
  (interactive)
  (find-file "~/Dropbox/Passwords.kdbx"))
(global-set-key (kbd "C-c s p") 'open-passwords)
;;; -----------------------latex----------------------------
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq-default TeX-engine 'luatex)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))

;;; --------------------------vterm-------------------------------
(unless (package-installed-p 'vterm)
  (package-install 'vterm))

;;; --------------------------------eshell-----------------------------------
(defun ecd ()
  "
  Sets the eshell directory to the current buffer

  Usage: M-x ecd 
  "
  (interactive)
  (let (
		(path (file-name-directory (or  (buffer-file-name) default-directory)))
		)
	(with-current-buffer "*eshell*"
	  (cd path)
	  (eshell-emit-prompt)))); Start small shell with SPC SPC
(defun start_small_shell()
  (interactive)
  (split-window-below)
  (windmove-down)
  (window-resize nil (- (/ (window-total-height) 2)))
  (eshell)
  (windmove-up)
  (ecd)
  (windmove-down)
  )

(define-key evil-normal-state-map (kbd "SPC SPC") 'start_small_shell)
;; aliases - see https://emacs.stackexchange.com/questions/48843/how-to-store-an-eshell-alias-in-init-el
;; Not recommended to do it this way but I wanted everything in this file
(defun eshell-add-aliases ()
  "Doc-string."
  (dolist (var '(("ipython" "ipython --simple-prompt -i --pprint $*")
				 (":q" "exit $*")
				 ("dotfiles" "/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME $*")
				 ("tp" "trash-put $*")))
	(add-to-list 'eshell-command-aliases-list var)))

(add-hook 'eshell-post-command-hook 'eshell-add-aliases)

(global-set-key (kbd "C-c s s" ) 'eshell)
;;; -------------------------------python-------------------------------------
;; Download elpy - requires python, jedi, black, autopep8, yapf(install w/ pip)
(unless (package-installed-p 'elpy)
  (package-install 'elpy))
;; Enable elpy
(elpy-enable)
;; Disable elpy vertical lines
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
;; Make default shell ipython if possible - requires ipython, pyreadline
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(setq python-shell-interpreter-args "--simple-prompt --pprint -i")
(setq python-shell-completion-native-enable nil)
(setq python-shell-enable-font-lock nil)
(setq elpy-rpc-virtualenv-path 'system) ; Don't use a venv by default


;;; -------------------------------c/c++/obj-c-------------------------------------
;; Bracket indentation
(setq c-default-style "bsd")
;; flycheck syntax check install
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode) ; enable
;; company-mode autocomplete
(unless (package-installed-p 'company)
  (package-install 'company))
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
;; Download irony - requires llvm and irony(yay -S irony-mode) and bear(compile database)
(unless (package-installed-p 'irony) ; autocomplete
  (package-install 'irony))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(unless (package-installed-p 'flycheck-irony) ; syntax checker
  (package-install 'flycheck-irony))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;;; ------------------------nixlang----------------------------
(unless (package-installed-p 'nix-mode)
  (package-install 'nix-mode))
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
