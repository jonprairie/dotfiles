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
 '(package-selected-packages
   (quote
    (helm-projectile evil-surround helm projectile magit iedit evil))))

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
    'relative-line-numbers)

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
(require 'powerline)
; (powerline-default-theme)
(global-evil-surround-mode 1)
(global-evil-tabs-mode t)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
;;(powerline-evil-vim-color-theme)
(display-time-mode t)

(company-mode t)
(evil-mode t)
;;(global-linum-mode t)
(helm-mode 1)
(which-function-mode 1)
(show-paren-mode 1)

(global-font-lock-mode 't)

(global-set-key (kbd "M-x") 'helm-M-x)
(evil-leader/set-key "mx" 'helm-M-x)
;;(evil-leader/set-key "e" 'evil-ace-jump-word-mode) ; ,e for Ace Jump (word)
(evil-leader/set-key "l" 'evil-ace-jump-line-mode) ; ,l for Ace Jump (line)
(evil-leader/set-key "x" 'evil-ace-jump-char-mode) ; ,x for Ace Jump (char)

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
(evil-leader/set-key "ev" '(lambda () (interactive)
			     (switch-to-buffer "c:/Users/jonnyp/AppData/Roaming/.emacs")))
(evil-leader/set-key "st" '(lambda () (interactive) (eval-buffer)))
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "C-u") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-d") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

(add-hook 'prog-mode-hook 'relative-line-numbers-mode t)
(add-hook 'prog-mode-hook 'line-number-mode t)
(add-hook 'prog-mode-hook 'column-number-mode t)

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
(key-chord-mode 1)

(scroll-bar-mode -1)
(setq make-backup-files nil)
;; helm settings (TAB in helm window for actions over selected items,
;; C-SPC to select items)
(require 'helm-config)
(require 'helm-misc)
(require 'projectile)
(require 'helm-projectile)
(require 'helm-locate)
;;(helm-projectile-on)
(projectile-mode t)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)

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
      helm-semantic-fuzzy-match             t)

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

(evil-leader/set-key "p" 'helm-projectile-find-file)

(setq ring-bell-function 'ignore)

(evil-leader/set-key "qq" '(lambda () (interactive) (cd "c:/Python27/Lib/site-packages/cts2")))
(evil-leader/set-key "qw" '(lambda () (interactive) (cd "c:/Users/jonnyp/Documents/src")))

(setq projectile-require-project-root nil)
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
        )
          projectile-globally-ignored-files))
(projectile-global-mode)

(setq projectile-use-git-grep 1)
(setq projectile-indexing-method 'alien)
