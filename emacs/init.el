;; show # of matches for search
;;; notes
;; Guiding principles: Reproducability and portability
;; Exwm - setup emacs to launch on boot
;; Install lsp stuff for all required langues(clang for cpp)
;; Please install git, vim, zip, etc
;; vterm - cmake, libtool-bin, libvterm
;; For spellcheck, ispell
;; For better projectile search? and dumbjump install ripgrep
;; For python development install python(package manager), jedi, black, autopep8, yapf, pyreadline, ipython(pip), flake8, rope
;; For c/c++/obj-c/etc install bear, ccls, a compiler
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
;; Ensure emacs doesn't write to init.eL
(setq custom-file (concat user-emacs-directory "/custom.el"))


;;; --------------------------vim-migration/general-editing------------------------
(defun install-package (package)
  (unless (package-installed-p package)
	(package-refresh-contents)
	(package-install package)))

(display-time)
(setq-default tab-width 4)
(setq-default tab-stop-list 4)
;; Autodetect indentation if possible

(install-package 'dtrt-indent)

(dtrt-indent-global-mode 1)
;; Download Evil
(install-package 'evil)

;; Visual vertical movement(for lines that wrap)
(setq evil-respect-visual-line-mode nil) ; Ideally it would be true but it's not working rn :(
(global-visual-line-mode)
;; allow c-u scrolling in evil
(setq evil-want-C-u-scroll t)
;; Enable Evil
;; required for evil-collection
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(require 'evil)
(evil-mode 1)
;; Enable evil collection
(install-package 'evil-collection)

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
(install-package 'undo-tree)

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
;; Not sure which of these is redundant
;; Makes lines relative and work w/ folding
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-mode 'visual)

;; which-key
(install-package 'which-key)

(which-key-mode)
;; snippets
(install-package 'yasnippet)

(require 'yasnippet)
(yas-global-mode 1)
;; Multiple cursors
(install-package 'evil-multiedit)

(require 'evil-multiedit)
(evil-multiedit-default-keybinds)
;; Symbol list
(install-package 'imenu-list)

(setq imenu-list-auto-resize t)
(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)
(setq imenu-list-focus-after-activation t)
;; Dumb Jump
(install-package 'dumb-jump)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; Warn when lines are too long
(require 'whitespace)
(setq whitespace-line-column 92) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)
;; Edit in firefox
(install-package 'atomic-chrome)

(require 'atomic-chrome)
(atomic-chrome-start-server)

;; Kill buffer then reopen
(defun refresh-buffer ()
  (interactive)
  (if (buffer-modified-p)
	  (message "please save file first")
	(let ((file-name buffer-file-name))
	  (kill-buffer)
	  (find-file file-name))))
(global-set-key (kbd "C-c r") 'refresh-buffer)

;; Show number of matches when searching
(install-package 'anzu)
(global-anzu-mode +1)

;;; ------------------------navigation---------------------------
;; Call nearest makefile in parent dir.
(defun make (command)
  (interactive
   (let* ((make-directory (locate-dominating-file (buffer-file-name)
												  "Makefile"))
		  (command (concat "make -k -C "
						   (shell-quote-argument make-directory))))
	 (list (compilation-read-command command))))
  (compile command))
;; ripgrep
(install-package 'rg)
(require 'rg)

(install-package 'fancy-dabbrev)
(global-set-key (kbd "C-<return>") 'fancy-dabbrev-expand)
;; Over tramp, use simple autocomplete
(add-hook
 'prog-mode-hook
 (lambda () (when (file-remote-p default-directory) (company-mode -1) (fancy-dabbrev-mode))))
;; Sentences don't need 2 spaces after.
(setf sentence-end-double-space nil)

;; Projectile project jumping

(install-package 'projectile)

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
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
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
(install-package 'switch-window)
(require 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(global-set-key (kbd "M-o") 'switch-window)
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
  (if (> (window-pixel-width) (window-pixel-height))
	  (split-and-follow-vertically)
	(split-and-follow-horizontally)))
(global-set-key (kbd "C-c <C-return>") 'split-along-longer-side)
;; Window resize/resizing
;; C-c b(big) h(horizontal)/v(vertical)
;; C-c s(small) h(horizontal)/v(vertical)
(define-key evil-normal-state-map (kbd "+") 'enlarge-window)
(define-key evil-normal-state-map (kbd "-") 'shrink-window)

										; fuzzy find ido
(install-package 'flx-ido)

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
(install-package 'ido-vertical-mode)

(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; flycheck syntax check install
(install-package 'flycheck)

(add-hook 'prog-mode-hook #'flycheck-mode)
;; company-mode autocomplete
(install-package 'company)
(require 'company)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
;;; ---------------------------aesthetics-------------------------
;; Disable toolbar/menubar/:q
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Theme
(install-package 'solarized-theme)

(install-package 'doom-themes)

(setq solarized-high-contrast-mode-line t) ; must be before load-theme i think
(load-theme 'solarized-dark t)

;;; ------------------------------org-mode-------------------------
;; Spellcheck
(add-hook 'org-mode-hook 'flyspell-mode)
;; pretty bullets :)
(install-package 'org-bullets)

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
(global-set-key (kbd "C-c f s" ) ;; Go to the home directory of dired
				(lambda () (interactive)
				  (find-file "/ssh:dghosef@myth.stanford.edu:")))
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

;;; ----------------------pdf--------------------------
(evil-define-key 'normal doc-view-minor-mode-map
  "h" 'doc-view-previous-page
  "j" (lambda () (interactive) (doc-view-next-line-or-next-page 5))
  "k" (lambda () (interactive) (doc-view-previous-line-or-previous-page 5))
  "l" 'doc-view-next-page)
(install-package 'let-alist)

(install-package 'tablist)

;;; --------------------------------magit--------------------------
(install-package 'magit)


;;; ----------------------------dired------------------------
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.trash")
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; autorefresh

;;; --------------------------eww--------------------
(defun eww-new ()
  "Open eww in new buffer"
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
	(switch-to-buffer (generate-new-buffer (concat "eww - " url)))
	(eww-mode)
	(eww url)))
(global-set-key (kbd "C-c C-s b" ) 'eww)
(global-set-key (kbd "C-c C-s B" ) 'eww-new)
(evil-collection-define-key 'normal 'eww-mode-map "O" 'eww-new)
(setq eww-search-prefix "https://www.google.com/search?q=")

;;; -----------------------latex----------------------------
;; Note - in general I like overleaf more. 
(install-package 'auctex)

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
(install-package 'vterm)

;; Disable evil so we can use vim mode that's inbuilt to the shell
(add-hook 'vterm-mode-hook
		  (lambda () (evil-set-initial-state 'vterm-mode 'emacs)))

;;; --------------------------------eshell-----------------------------------
(global-set-key (kbd "C-c C-s s" ) 'eshell)
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
  (window-resize nil (/ (window-total-height) 2))
  (windmove-down)
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
				 ("ff" "find-file $")
				 ("tp" "trash-put $*")))
	(add-to-list 'eshell-command-aliases-list var)))

(add-hook 'eshell-post-command-hook 'eshell-add-aliases)

;; Disable company mode for eshell over tramp
(add-hook
 'eshell-mode-hook
 (lambda () (when (file-remote-p default-directory) (company-mode -1))))


;; autocomplete for eshell :)
(install-package 'company-shell)
(add-hook
 'eshell-mode-hook
 (lambda () (add-to-list 'company-backends 'company-shell)))


(install-package 'eglot)
(add-hook 'prog-mode-hook 'eglot-ensure)
;;; -------------------------------python-------------------------------------
;; Download elpy - requires python, jedi, black, autopep8, yapf(install w/ pip)
(install-package 'elpy)

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


;;; --------------------------------julia--------------------------------------
(install-package 'julia-mode)
(require 'julia-mode)
(install-package 'eglot-jl)
(eglot-jl-init)
;;; -------------------------------c/cpp/c++/obj-c-------------------------------------
;; Bracket indentation
(setq c-default-style "bsd")
;; M-[ is insert new {} with indentation and stuff
(defun insert_brackets ()
  (interactive)
  (if (not (looking-back " "))
	  (insert " "))
  (insert "{")
  (indent-for-tab-command)
  (insert "\n")
  (indent-for-tab-command)
  (insert "\n")
  (indent-for-tab-command)
  (insert "}")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command)
  )
(global-set-key (kbd "M-RET") 'insert_brackets)
;; Show function definitions below
(install-package 'c-eldoc)
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;; ------------------------nixlang---------------------------
(install-package 'nix-mode)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;;; --------------------------flex-------------------------
(when (not (file-exists-p "~/.emacs.d/flex-mode"))
  (shell-command "git clone https://github.com/manateelazycat/flex ~/.emacs.d/flex-mode"))
(add-to-list 'load-path "~/.emacs.d/flex-mode")
(require 'flex)

(add-to-list 'auto-mode-alist '("\\.flex$" . flex-mode))
(autoload 'flex-mode "flex")

;;; -----------------------bison------------------------
(when (not (file-exists-p "~/.emacs.d/bison-mode"))
  (shell-command "git clone https://github.com/Wilfred/bison-mode ~/.emacs.d/bison-mode"))
(add-to-list 'load-path "~/.emacs.d/bison-mode")
(require 'bison-mode)

(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))
(autoload 'flex-mode "bison-mode")

;;; -----------------diminish--------------------------
(install-package 'diminish)
(diminish 'projectile-mode)
(diminish 'flycheck-mode)
(diminish 'undo-tree-mode)
(diminish 'anzu-mode)

;;;-----------------------------exwm----------------------------
;; Try not to have this section affect regular emacs boots.
;; Very much from https://github.com/ch11ng/exwm/wiki/Configuration-Example
;; Then tried to have alot of my i3 features/config here
;; I don't think I will ever switch to another wm/de
(install-package `exwm)
;; Buffer movement follows mouse. Must be called before exwm starts
(setq mouse-autoselect-window t
	  focus-follows-mouse t)
;; Required for resizing windows w/ mouse
(window-divider-mode)
;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
		  (lambda ()
			(exwm-workspace-rename-buffer exwm-class-name)))
(require 'exwm)
(require 'exwm-config)
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(setq exwm-systemtray-height 15)
(exwm-config-ido)
(setq exwm-workspace-number 10)
;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
										;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows except for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
		  (lambda ()
			(unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
						(string= "gimp" exwm-instance-name))
			  (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
		  (lambda ()
			(when (or (not exwm-instance-name)
					  (string-prefix-p "sun-awt-X11-" exwm-instance-name)
					  (string= "gimp" exwm-instance-name))
			  (exwm-workspace-rename-buffer exwm-title))))
;; switch to other buffer sorta like vim C-6
(defun switch-to-previous-buffer ()
  (interactive)
  (evil-switch-to-windows-last-buffer))
;; go to most recently used window(will toggle if used successively)
(defun last-window ()
  (interactive)
  (let ((win (get-mru-window nil nil t)))
    (when win (select-window win))))
;; spawn new process if buf doesn't exist otherwise switch to buf
;; If we're already on that window, go back to most recently used  window
(defun spawn-or-switch (buf cmd)
  (interactive)
  (if (get-buffer buf)
	  ;; Either reopen the buffer if it's not already open or jump to the window
	  ;; if it is. If we're already on that buffer, go to mru window
	  (if (get-buffer-window buf)
		  (if (string= (buffer-name) buf)
			  (last-window)
			(select-window (get-buffer-window buf)))
		(switch-to-buffer buf))
	(start-process-shell-command cmd nil cmd)))
(defun spawn (cmd)
  (start-process-shell-command cmd nil cmd))
;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
	  `(
		;; Bind "s-w" to switch workspace interactively.
		([?\s-w] . exwm-workspace-switch)
		;; S-hjkl to emulate my i3 setup
		([?\s-h] . windmove-left)
		([?\s-j] . windmove-down)
		([?\s-k] . windmove-up)
		([?\s-l] . windmove-right)
		([?\s-n] . (lambda ()
					 (interactive)
					 (split-along-longer-side)
					 (if (not (get-buffer "Thunderbird"))
						 (spawn "thunderbird"))))
		([?\s-\[] . previous-buffer)
		([?\s-\]] . next-buffer)
		([?\s-o] . switch-to-previous-buffer)
		;; S-i to toggle between the modes
		([?\s-i] . exwm-input-toggle-keyboard)
		;; toggle fullscreen
		([?\s-f] . exwm-layout-toggle-fullscreen)
		;; go to most recent window
		([?\s-`] . switch-to-previous-buffer)
		;; 'S-s-N': Move window to, and switch to, a certain workspace.
		,@(cl-mapcar (lambda (c n)
					   `(,(kbd (format "s-%c" c)) .
						 (lambda ()
						   (interactive)
						   (exwm-workspace-move-window ,n)
						   (exwm-workspace-switch ,n))))
					 '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
					 ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
					 (number-sequence 0 9))
		;; Bind "s-0" to "s-9" to switch to a workspace by its index.
		,@(mapcar (lambda (i)
					`(,(kbd (format "s-%d" i)) .
					  (lambda ()
						(interactive)
						(exwm-workspace-switch-create ,i))))
				  (number-sequence 0 9))
		;; Bind "s-r" to launch applications ('M-&' also works if the output
		;; buffer does not bother you). I used r because I'm used to doing
		;; r for rofi
		([?\s-r] . (lambda (command)
					 (interactive (list (read-shell-command "$ ")))
					 (spawn command)))
		;; Bind "s-g" to i3lock
		([?\s-g] . (lambda ()
					 (interactive)
					 (shell-command "i3lock --color=#000000 --show-failed-attempts")))
		;; Bind "s-x" to scrot
		([?\s-x] . (lambda ()
					 (interactive)
					 (spawn "scrot -s '%Y-%m-%d_$wx$h_scrot.png' -e 'mv $f ~/Desktop/'")))
		;; Bind "s-+"/"s--" to volume
		([?\s-=] . (lambda ()
					 (interactive)
					 (spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")))
		;; Bind "s-+"/"s--" to volume
		([?\s--] . (lambda ()
					 (interactive)
					 (spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")))
		;; spotify play/pause
		([?\s-\;] . (lambda ()
					  (interactive)
					  (spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")))
		([?\s-'] . (lambda ()
					 (interactive)
					 (spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")))
		([?\s-:] . (lambda ()
					 (interactive)
					 (spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")))
		;; Shortcuts for apps
		;; w for web
		([?\s-b] . (lambda ()
					 (interactive)
					 (spawn-or-switch "Firefox" "firefox")))
		;; spotify
		([?\s-s] . (lambda ()
					 (interactive)
					 (spawn-or-switch "Spotify" "spotify")))
		;; Keepassxc
		([?\s-p] . (lambda ()
					 (interactive)
					 (spawn-or-switch "KeePassXC" "keepassxc")))
		;; Discord
		([?\s-d] . (lambda ()
					 (interactive)
					 (spawn-or-switch "discord" "Discord")))
		;; Thunderbird
		([?\s-m] . (lambda ()
					 (interactive)
					 (spawn-or-switch "Thunderbird" "thunderbird")))
		;; eshell
		;; t for terminal(i know, i know, eshell isn't really a teminal)
		([?\s-t] . (lambda ()
					 (interactive)
					 (if (get-buffer "*eshell*")
						 (switch-to-buffer "*eshell*")
					   (eshell))))
		))
;; Mainly to allow other apps to keep native shortcuts
;; and basic vim motions :)
;; Try to enter char mode as not often as possible
(exwm-input-set-simulation-keys
 '(([?\s-c] . C-c) ; copy
   ([?\s-v] . C-v) ; paste
   ([?\s-u] . C-l) ; For url bar access
   ([?\s-w] . C-w) ; close tab
   ([?\C-l] . C-l))) ; access url
(define-key exwm-mode-map [?\M-o] 'switch-window)
(provide 'init)
;;; init ends here
