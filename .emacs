;; for emacs whitout X
;; Kemacs path
(add-to-list 'load-path "~/.Kemacs/")

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

;; for all
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)

(global-font-lock-mode t)

(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode t)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(setq scroll-margin 3 scroll-conservatively 10000)

(setq make-backup-files nil)

(show-paren-mode)
(setq show-paren-style 'parenthesis)

(setq-default kill-whole-line t)
(setq-default track-eol t)
(setq Man-notify-method 'pushy)

;; for hippie-expand
(setq hippie-expand-try-functions-list 
      '(
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; for keys
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [select] 'end-of-buffer)
(global-set-key [f2] 'eshell)
(global-set-key [f3] 'repeat-complex-command)
(global-set-key [f4] 'other-window)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'gdb)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'undo)
(global-set-key [(meta l)] 'goto-line)
(global-set-key [(meta -)] 'set-mark-command)
;; (global-set-key [(meta /)] 'hippie-expand)
(global-set-key [up] 'windmove-up)
(global-set-key [down] 'windmove-down)
(global-set-key [left] 'windmove-left)
(global-set-key [right] 'windmove-right)

;; for c/c++
(add-hook 'c-mode-hook
          '(lambda()
             (c-toggle-auto-state)))
(add-hook 'c-mode-hook
          '(lambda()
             (c-toggle-auto-newline)))
(add-hook 'c-mode-hook
          '(lambda()
             (c-toggle-auto-hungry-state)))
;; Baidu style
(require 'ecom-c-style)
(add-hook 'c-mode-common-hook 'ecom-set-c-style)
(add-hook 'c-mode-common-hook 'ecom-make-newline-indent)
(setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))

;; yasnippet
(require 'yasnippet-bundle)

;; Auto Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories ".Kemacs/dict/")
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")

;; CEDET
(load-file "~/.Kemacs/cedet/common/cedet.el")

(semantic-load-enable-code-helpers)
(global-semantic-tag-folding-mode 1)

(require 'semantic-ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(require 'semantic-gcc)

(setq semanticdb-project-roots (list (expand-file-name "/")))
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))
(require 'semantic-c nil 'noerror)
(let ((include-dirs cedet-user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; Key bindings
(defun my-cedet-hook ()
  (global-set-key (kbd "M-/") 'senator-complete-symbol)
  (global-set-key [f11] 'semantic-ia-fast-jump)
  (global-set-key [f12]
                  (lambda ()
                    (interactive)
                    (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
                        (error "Semantic Bookmark ring is currently empty"))
                    (let* ((ring (oref semantic-mru-bookmark-ring ring))
                           (alist (semantic-mrub-ring-to-assoc-list ring))
                           (first (cdr (car alist))))
                      (if (semantic-equivalent-tag-p (oref first tag)
                                                     (semantic-current-tag))
                          (setq first (cdr (car (cdr alist)))))
                      (semantic-mrub-switch-tags first)))))
(add-hook 'semantic-init-hooks 'my-cedet-hook)

;; (defun my-c-mode-cedet-hook ()
;;   (local-set-key "." 'semantic-complete-self-insert)
;;   (local-set-key ">" 'semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; ECB
(setq stack-trace-on-error nil)
(add-to-list 'load-path "~/.Kemacs/ecb")
(require 'ecb)
(setq ecb-auto-activate t)
(setq ecb-tip-of-the-day nil)

(defun my-ecb-active-or-deactive ()
  (interactive)
  (if ecb-minor-mode
      (ecb-deactivate)
    (ecb-activate)))
(global-set-key [f1] 'my-ecb-active-or-deactive)

(global-set-key (kbd "C-c 1") 'ecb-goto-window-directories)
(global-set-key (kbd "C-c 2") 'ecb-goto-window-sources)
(global-set-key (kbd "C-c 3") 'ecb-goto-window-methods)
(global-set-key (kbd "C-c 4") 'ecb-goto-window-history)
(global-set-key (kbd "C-c e") 'ecb-goto-window-edit-last)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-windows-width 0.2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; for shell
(ansi-color-for-comint-mode-on)
(defun eshell-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10)
  (setq scroll-step 1)
  (setq scroll-margin 0))
(add-hook 'eshell-mode-hook 'eshell-scroll-conservatively)

;; tabbar
(require 'tabbar)
(tabbar-mode 1)
(global-set-key [(meta \9)] 'tabbar-backward-tab)
(global-set-key [(meta \0)] 'tabbar-forward-tab)
(global-set-key [(meta \7)] 'tabbar-backward-group)
(global-set-key [(meta \8)] 'tabbar-forward-group)

;; for color theme
(require 'color-theme)
(color-theme-initialize)

;; for php
(require 'php-mode)
(color-theme-arjen)

;; for codec
(setq default-buffer-file-coding-system 'gbk)
(prefer-coding-system 'utf-8)

;; for GUI
(tool-bar-mode 0)
(scroll-bar-mode 0)
(set-default-font "Dejavu Sans mono-14")
