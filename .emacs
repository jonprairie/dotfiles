(setq debug-on-error t)
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(org-agenda-files (quote ("c:/Users/e018462/Documents/src/todo.org")))
 '(package-selected-packages
   (quote
    (caps-lock fill-column-indicator helm-projectile evil-surround helm projectile magit iedit evil))))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(package-initialize)

(ensure-package-installed
    'iedit
    'magit
    'evil
    'evil-escape
    'evil-surround
    'evil-leader
    'evil-tabs
    'flycheck
    'key-chord
    'powerline
    'projectile
    'helm
    'helm-projectile
    'company
    'yasnippet
    'relative-line-numbers
    'jabber
    'fill-column-indicator
    'caps-lock)

(add-to-list 'load-path "~/.emacs.d/cobol-mode")
(add-to-list 'load-path "~/.emacs.d/rexx-mode")
(autoload 'cobol-mode "cobol-mode" "Major mode for highlighting COBOL files." t nil)
(setq auto-mode-alist
   (append
     '(("\\.cob\\'" . cobol-mode)
       ("\\.cbl\\'" . cobol-mode)
       ("\\.cpy\\'" . cobol-mode))
    auto-mode-alist))
(add-hook 'cobol-mode-hook (lambda () (toggle-truncate-lines)))

(require 'fill-column-indicator)

(add-hook 'evil-insert-state-exit-hook
	  (lambda ()
	    (if caps-lock-mode
		(caps-lock-mode -1))))
;(add-hook 'evil-normal-state-entry-hook
;	  (lambda ()
;	    (if caps-lock-mode
;		(caps-lock-mode))))

;;(defvar my-linum-format-string "%3d")

;;(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
;;
;;;; Use relative line numbers
;;(defun my-linum-get-format-string ()
  ;;(let* ((width (1+ (length (number-to-string
                             ;;(count-lines (point-min) (point-max))))))
         ;;(format (concat "%" (number-to-string width) "d")))
    ;;(setq my-linum-format-string format)))
;;
;;(defvar my-linum-current-line-number 0)
;;
;;(setq linum-format 'my-linum-relative-line-numbers)
;;
;;(defun my-linum-relative-line-numbers (line-number)
  ;;(let ((offset (abs (- line-number my-linum-current-line-number))))
    ;;(propertize (format my-linum-format-string offset) 'face 'linum)))
;;
;;(defadvice linum-update (around my-linum-update)
  ;;(let ((my-linum-current-line-number (line-number-at-pos)))
    ;;ad-d-o-it))
;;(ad-activate 'linum-update)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-leader/in-all-states 1)
(setq ace-jump-word-mode-use-query-char nil)

(load-theme 'deeper-blue t)

(require 'evil)
(require 'evil-surround)
(require 'helm-config)
(require 'helm-misc)
(require 'projectile)
(require 'helm-projectile)
(require 'helm-locate)
;(require 'evil-tabs)
;(global-evil-tabs-mode t)
(require 'powerline)
(powerline-default-theme)
(global-evil-surround-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
; (powerline-evil-vim-color-theme)
(display-time-mode t)

(company-mode t)
(evil-mode t)
;;(global-linum-mode t)
(which-function-mode 1)
(show-paren-mode 1)

(global-font-lock-mode 't)

(setq evil-mode-mappings
      (list
       "i" 'evil-insert-state-map
       "e" 'evil-emacs-state-map
       "n" 'evil-normal-state-map
       "v" 'evil-visual-state-map
       "m" 'evil-motion-state-map
       "o" 'evil-operator-state-map
       "u" 'evil-outer-text-objects-map
       "r" 'evil-inner-text-objects-map
       "p" 'evil-replace-state-map))

(lax-plist-get evil-mode-mappings (char-to-string ?n))

(defmacro evil-def-multi-keys (state-list binding fn)
  "Binds a key combination 'binding' to a function 'fn' in multiple evil-states at once. 

'state-list' should be a string consisting of one or more of the following chars:
       'i' - evil-insert-state-map
       'e' - evil-emacs-state-map
       'n' - evil-normal-state-map
       'v' - evil-visual-state-map
       'm' - evil-motion-state-map
       'o' - evil-operator-state-map
       'u' - evil-outer-text-objects-map
       'r' - evil-inner-text-objects-map
       'p' - evil-replace-state-map

for example:

(evil-def-multi-keys \"ienv\" (kbd \"<f5>\") \'evil-prev-buffer)

will bind <f5> to evil-prev-buffer in insert, emacs, normal and visual modes"
  `(progn
    ,@(mapcar
	(lambda (state-key-char)
	    `(define-key
		,(lax-plist-get
		    evil-mode-mappings
		    (char-to-string state-key-char))
		,binding
		,fn))
	(string-to-list state-list))))

(evil-def-multi-keys "ienv" (kbd "<f5>") 'evil-prev-buffer)
(evil-def-multi-keys "ienv" (kbd "<f6>") 'evil-next-buffer)

(global-set-key (kbd "M-x") 'helm-M-x)
(evil-leader/set-key "mx" 'helm-M-x)
(evil-leader/set-key "g" 'magit-status)
;;(evil-leader/set-key "e" 'evil-ace-jump-word-mode) ; ,e for Ace Jump (word)
;;(evil-leader/set-key "l" 'evil-ace-jump-line-mode) ; ,l for Ace Jump (line)
;;(evil-leader/set-key "x" 'evil-ace-jump-char-mode) ; ,x for Ace Jump (char)

(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-operator-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "H") 'evil-digit-argument-or-evil-beginning-of-line)
(define-key evil-visual-state-map (kbd "H") 'evil-digit-argument-or-evil-beginning-of-line)
(define-key evil-operator-state-map (kbd "H") 'evil-digit-argument-or-evil-beginning-of-line)
(evil-leader/set-key "j" 'evil-window-down)
(evil-leader/set-key "k" 'evil-window-up)
(evil-leader/set-key "h" 'evil-window-left)
(evil-leader/set-key "l" 'evil-window-right)
(evil-leader/set-key "st" '(lambda () (interactive) (eval-buffer)))
(evil-leader/set-key "ev" 'eval-last-sexp) 
(define-key evil-normal-state-map (kbd "j") 'evil-next-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-line)
(define-key evil-normal-state-map (kbd "<f7>") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "<f8>") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

(define-key evil-normal-state-map (kbd "S-<f7>") 'evil-goto-first-line)
(define-key evil-normal-state-map (kbd "S-<f8>") 'evil-goto-line)
(define-key evil-visual-state-map (kbd "S-<f7>") 'evil-goto-first-line)
(define-key evil-visual-state-map (kbd "S-<f8>") 'evil-goto-line)
(define-key evil-insert-state-map (kbd "S-<f7>") 'evil-goto-first-line)
(define-key evil-insert-state-map (kbd "S-<f8>") 'evil-goto-line)

(define-key evil-insert-state-map (kbd "C-;")
  'caps-lock-mode) 

(line-number-mode t)
(relative-line-numbers-mode t)
(column-number-mode t)
(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)
(add-hook 'rexx-mode-hook 'relative-line-numbers-mode t)

;(defun define-evil-key-map (key-map fn modes)
;  (if (string-contains-char modes "i")
;      (define-key evil-insert-state-map (kbd key-map) 'fn)))
;
;(defun string-contains-char-iter (str c counter)	
;  (if (= (seq-length str) counter)
;      nil
;    (if (= (elt str counter) (string-to-char c))
;	"true"
;      (string-contains-char-iter str c (+ 1 counter)))))
;(defun string-contains-char (str c)
;  (string-contains-char-iter str c 0))
;
;(string-contains-char "hello" "o")
;
;
;(string-contains-char "hello" "o")
;(car "hello")
;(nth 4 "hello")
;(char-equal 'c' 'c')
;(seq-length "hello")
;(char-to-string (elt "hello" 1))

;; flycheck
;;(package 'flycheck)
; (add-hook 'after-init-hook #'global-flycheck-mode)

; (after 'flycheck
;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;   (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;   (setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
;   (setq flycheck-standard-error-navigation nil))

; (global-flycheck-mode t)

; ;; flycheck errors on a tooltip (doesnt work on console)
; (when (display-graphic-p (selected-frame))
;   (eval-after-load 'flycheck
;     '(custom-set-variables
;       '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(add-hook 'after-init-hook 'global-company-mode)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "Jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "Jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jK" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jK" 'evil-normal-state)
(key-chord-define evil-insert-state-map "JK" 'evil-normal-state)
(key-chord-define evil-visual-state-map "JK" 'evil-normal-state)

(key-chord-mode 1)

(scroll-bar-mode -1)
(setq make-backup-files nil)
;; helm settings (TAB in helm window for actions over selected items,
;; C-SPC to select items)
;;(helm-projectile-on)
(projectile-mode t)

(setq helm-M-x-fuzzy-match                  t
      helm-bookmark-show-location           t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match           t
      helm-imenu-fuzzy-match                t
      helm-mode-fuzzy-match                 t
      helm-locate-fuzzy-match               t 
      helm-quick-update                     t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-split-window-in-side-p           t
      ;helm-always-two-windows               t
      helm-echo-input-in-header-line        t)

;;(after 'projectile
;;  (package 'helm-projectile))

;;(defun helm-my-buffers ()
;;  (interactive)
;;  (let ((helm-ff-transformer-show-only-basename nil))
;;  (helm-other-buffer '(helm-c-source-buffers-list
;;                       helm-c-source-elscreen
;;                       helm-c-source-projectile-files-list
;;                       helm-c-source-ctags
;;                       helm-c-source-recentf
;;                       helm-c-source-locate)
;;                     "*helm-my-buffers*")))

(evil-leader/set-key "p" 'helm-projectile-find-file-in-known-projects)
(evil-leader/set-key "o" 'helm-buffers-list)
(evil-leader/set-key "v" 'helm-show-kill-ring)
(evil-leader/set-key "i" 'helm-comint-input-ring)

(evil-define-motion evil-next-line-and-scroll-to-top (count)
  "Move the cursor count lines down, then set top of buffer to current line"
  :type line
  (evil-next-line count)
  (evil-scroll-line-to-top nil))

(evil-define-motion evil-prev-line-and-scroll-to-top (count)
  "Move the cursor count lines up, then set top of buffer to current line"
  :type line
  (evil-previous-line count)
  (evil-scroll-line-to-top nil))

(evil-def-multi-keys "env" (kbd "]") 'evil-next-line-and-scroll-to-top)

(evil-def-multi-keys "env" (kbd "[") 'evil-prev-line-and-scroll-to-top)

(setq ring-bell-function 'ignore)

(evil-leader/set-key "qq" '(lambda () (interactive) (cd "c:/users/e018462/appdata/Roaming")))
(evil-leader/set-key "qw" '(lambda () (interactive) (cd "c:/Users/e018462/Documents/src")))

;(setq projectile-require-project-root nil)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-directories
      (append '(
        ".git"
        ".svn"
        "out"
        "repl"
        "target"
        "venv"
	"node_modules"
	"platforms"
        )
          projectile-globally-ignored-directories))
(setq projectile-globally-ignored-files
      (append '(
        ".DS_Store"
        "*.gz"
        "*.pyc"
        "*.jar"
        "*.tar.gz"
        "*.tgz"
        "*.zip"
	"*.*~"
	"*.swp"
	"*.min.*"
        )
          projectile-globally-ignored-files))
(projectile-global-mode)

(setq projectile-use-git-grep 1)
(setq projectile-indexing-method 'alien)
;(setq projectile-indexing-method 'native)

;; Org-Mode
(add-hook 'org-mode-hook
    (lambda()
        (relative-line-numbers-mode t)
        (org-indent-mode t)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-fill-mode -1)

(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))
(setq org-tag-alist
      '((:startgroup . nil)
	("@work" .?w)
	("@home" . ?h)
	("@errand" . ?e)
	(:endgroup . nil)
	("@phone" . ?p)
	("@laptop" . ?l)))

(setq org-agenda-text-search-extra-files '(agenda-archives))
(setq org-enforce-todo-dependencies t)
(setq org-log-done (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)

(helm-mode 1)
(require 'jabber)
;(setq jabber-invalid-certificate-servers '("cup1.aoins.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq evil-state-hash (make-hash-table :test 'equal))
(puthash "n" 'evil-normal-state-map evil-state-hash)
(message (symbol-name (gethash "n" evil-state-hash)))

;(mapcar
; #'(lambda (x)
;     (symbol-name (gethash (string x) evil-state-hash)))
; "nnn")
