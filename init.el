;;; Notes
; Please install git, vim, etc
; For spellcheck, ispell
; For pdf editing follow https://github.com/politza/pdf-tools. Comment out pdf-loader-install if necessary
; For better projectile search install ripgrep
; For python development install python(package manager), jedi, black, autopep8, yapf, pyreadline, ipython(pip)
; For c/c++/obj-c/etc install llvm, irony-server, bear
; For taking notes/drawing the program "drawing"
; For latex preview dvipng and 'latex'. For export also install zip
; Enable debug messages for problems with this file
; (setq debug-on-error t)
;;; ---------------------------Setup package manager-------------------------
; Set up package.el to work with MELPA
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)


;;; --------------------------Vim-migration/general-editing------------------------
(setq-default tab-width 4)
(setq-default tab-stop-list 4)
; Autodetect indentation if possible

(unless (package-installed-p 'dtrt-indent)
  (package-install 'dtrt-indent))
; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
; allow c-u scrolling in evil
(setq evil-want-C-u-scroll t)
; Enable Evil
; required for evil-collection
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(require 'evil)
(evil-mode 1)
; Enable evil collection
(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(evil-collection-init
)
(setq evil-search-module 'evil-search)
;; Disable evil unwanted org indentation
(setq evil-auto-indent nil)
; Autosave backup directory
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
; Autorefresh files
(global-auto-revert-mode)
; set leader key in normal state
; regular undo tree
(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(require 'undo-tree)
(global-undo-tree-mode)
(undo-tree-mode 1)
; Persistent undo
(setq undo-tree-auto-save-history 1)
; undo
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
; redo
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
; Highlight matching ({[
(show-paren-mode 1)
; Redo
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


; Relative numbering
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers 'visual)
(setq display-line-numbers 'visual)

; which-key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)
; snippets
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(require 'yasnippet)
(yas-global-mode 1)


;;; ------------------------Navigation---------------------------
; tabs shortcuts
(global-set-key (kbd "M-t") 'tab-new)
(global-set-key (kbd "M-d") 'tab-close)
(global-set-key (kbd "M-1") (lambda () (interactive) (tab-bar-select-tab 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (tab-bar-select-tab 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (tab-bar-select-tab 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (tab-bar-select-tab 4)))
(global-set-key (kbd "M-5") (lambda () (interactive) (tab-bar-select-tab 5)))
(global-set-key (kbd "M-6") (lambda () (interactive) (tab-bar-select-tab 6)))
(global-set-key (kbd "M-7") (lambda () (interactive) (tab-bar-select-tab 7)))
(global-set-key (kbd "M-8") (lambda () (interactive) (tab-bar-select-tab 8)))
(global-set-key (kbd "M-9") (lambda () (interactive) (tab-bar-select-tab 9)))
(global-set-key (kbd "M-0") (lambda () (interactive) (tab-bar-select-tab 10)))

; Projectile project jumping
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-enable-caching t)
; See https://emacs.stackexchange.com/questions/16497/how-to-exclude-files-from-projectile
(setq projectile-enable-caching t)
; Go to next and previous buffer
(global-set-key (kbd "C-{") 'previous-buffer)
(global-set-key (kbd "C-}") 'next-buffer)
;;; ---------------------------Aesthetics-------------------------
; Disable toolbar/menubar/:q
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

; Theme
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(load-theme 'solarized-dark t)
; Highlight long code
(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode) ; Only when we're actually coding

;;; ------------------------------ORG-mode-------------------------
; Live preview in eww
(unless (package-installed-p 'org-preview-html)
  (package-install 'org-preview-html))
; pretty bullets :)
(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-with-inline-images t) ; show inline images
; Start drawing software and insert new drawing into current line
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

; Bindings to common iles
(global-set-key (kbd "<f10>" )
				(lambda () (interactive)
				  (find-file (concat org-directory "lists.org"))))
(global-set-key (kbd "<f9>" )
				(lambda () (interactive)
				  (find-file "~/.config/nixpkgs/init.el")))
(global-set-key (kbd "<f8>" )
				(lambda () (interactive)
				  (find-file "~/.config/nixpkgs/home.nix")))
; Snippets
(yas-define-snippets 'org-mode
					 '(("bmatrix" "\\begin{bmatrix}\n$1\n\\end{bmatrix}" "Insert a latex bmatrix")
					   ("bmx" "\\begin{bmatrix}$1\\end{bmatrix}$2" "Insert a latex bmatrix")
					   ("begin" "\\begin{$1}\n$2\n\\end{$1}" "Begin stuff")
					   ("frc" "\\frac{$1}\\{$2}" "Insert a latex fraction")
					   ("box" "\\begin{tcolorbox}$1\\end{tcolorbox}" "tcolorbox - latex")
					   ("*" "\\cdot" "dot product")
					   ("equation" "\\begin{equation*}\n\\begin{align*}\n$1\n\\end{align*}\n\\end{equation*}\n" "Insert latex code")))
(setq org-agenda-restore-windows-after-quit t)
(setq org-default-notes-file (concat org-directory "lists.org"))
(global-set-key (kbd "C-c c") 'org-capture)
; Required to get org and yas working.
; see https://orgmode.org/manual/Conflicts.html#Conflicts
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

;;; ----------------------PDF--------------------------
; If pdftools isn't installed it uses doc-view mode
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

;;; -----------------------Latex----------------------------
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


;;; --------------------------------Magit--------------------------
(unless (package-installed-p 'magit)
  (package-install 'magit))

;;; ----------------------------Dired------------------------
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.trash")

;;; --------------------------------EShell-----------------------------------
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
; aliases - see https://emacs.stackexchange.com/questions/48843/how-to-store-an-eshell-alias-in-init-el
; Not recommended to do it this way but I wanted everything in this file
(defun eshell-add-aliases ()
  "Doc-string."
  (dolist (var '(("ipython" "ipython --simple-prompt -i --pprint $*")
				 (":q" "exit $*")
				 ("dotfiles" "/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME $*")
				 ("tp" "trash-put $*")))
	(add-to-list 'eshell-command-aliases-list var)))

(add-hook 'eshell-post-command-hook 'eshell-add-aliases)

;;; -------------------------------PYTHON-------------------------------------
; Download elpy - requires python, jedi, black, autopep8, yapf(install w/ pip)
(unless (package-installed-p 'elpy)
  (package-install 'elpy))
; Enable elpy
(elpy-enable)
; Disable elpy vertical lines
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
; Make default shell ipython if possible - requires ipython, pyreadline
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))
(setq python-shell-interpreter-args "--simple-prompt --pprint -i")
(setq python-shell-completion-native-enable nil)
(setq python-shell-enable-font-lock nil)


;;; -------------------------------C/C++/OBJ-C-------------------------------------
; flycheck syntax check install
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode) ; enable
; company-mode autocomplete
(unless (package-installed-p 'company)
  (package-install 'company))
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
; Download irony - requires llvm and irony(yay -S irony-mode) and bear(compile database)
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


;;; ------------------------NIXlang----------------------------
(unless (package-installed-p 'nix-mode) ; syntax checker
  (package-install 'nix-mode))
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
