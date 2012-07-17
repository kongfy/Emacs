;; Kemacs path
(add-to-list 'load-path ".Kemacs/")

;; tabbar
(require 'tabbar)
(tabbar-mode 1)
(global-set-key [C-f7] 'tabbar-backward)
(global-set-key [C-f8] 'tabbar-forward)

;; yasnippet
(require 'yasnippet-bundle)

;; Auto Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories ".Kemacs/dict/")
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
;; (set-face-background 'ac-candidate-face "lightgray")
;; (set-face-underline 'ac-candidate-face "darkgray")
;; (set-face-background 'ac-selection-face "steelblue")

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
(load-file ".Kemacs/cedet/common/cedet.el")

(semantic-load-enable-code-helpers)

(global-semantic-tag-folding-mode 1)

(require 'semantic-ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(require 'semantic-gcc)

;; (setq semanticdb-project-roots (list (expand-file-name "/")))
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))
(defconst cedet-win32-include-dirs
  (list "C:/MinGW/include"
        "C:/MinGW/include/c++/3.4.5"
        "C:/MinGW/include/c++/3.4.5/mingw32"
        "C:/MinGW/include/c++/3.4.5/backward"
        "C:/MinGW/lib/gcc/mingw32/3.4.5/include"
        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))
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

;; ECB
(add-to-list 'load-path ".Kemacs/ecb")
(require 'ecb)
(setq ecb-auto-activate t)
(setq ecb-tip-of-the-day nil)

(defun my-ecb-active-or-deactive ()
  (interactive)
  (if ecb-minor-mode
      (ecb-deactivate)
    (ecb-activate)))

(global-set-key [f10] 'my-ecb-active-or-deactive)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("~/Documents/"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; AucTex
(add-to-list 'load-path ".Kemacs/auctex/site-lisp/site-start.d")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(if (string-equal system-type "windows-nt")
    (require 'tex-mik))
(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'xetex       ; use xelatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))


;; ;; Key bindings
(defun my-cedet-hook ()
  (local-set-key (kbd "M-/") 'semantic-ia-complete-symbol-menu)
  (local-set-key [f11] 'semantic-ia-fast-jump)
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
(add-hook 'c-mode-common-hook 'my-cedet-hook)

;; (defun my-c-mode-cedet-hook ()
;;   (local-set-key "." 'semantic-complete-self-insert)
;;   (local-set-key ">" 'semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; for shell
(ansi-color-for-comint-mode-on)
(defun eshell-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10)
  (setq scroll-step 1)
  (setq scroll-margin 0))
(add-hook 'eshell-mode-hook 'eshell-scroll-conservatively)

;; GUI
(tool-bar-mode nil)			;close tool bar
(customize-set-variable 'scroll-bar-mode 'right)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 10)

;; for all
(fset 'yes-or-no-p 'y-or-n-p)
(setq default-major-mode 'text-mode)
(setq frame-title-format "emacs@%b")
(setq inhibit-startup-message t)

(global-font-lock-mode t)

(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(setq scroll-margin 3 scroll-conservatively 10000)

(setq make-backup-files nil)

(show-paren-mode)
(setq show-paren-style 'parenthesis)

;; (setq skeleton-pair t)
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

(setq-default kill-whole-line t)

(setq-default track-eol t)
(setq Man-notify-method 'pushy)

(set-default-font "Dejavu Sans mono-12")

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
(global-set-key [end] 'end-of-buffer)
(global-set-key [f2] 'eshell)
(global-set-key [f3] 'repeat-complex-command)
(global-set-key [f4] 'other-window)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'gdb)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'undo)
(global-set-key (kbd "C-;") 'goto-line)
(global-set-key (kbd "C-'") 'set-mark-command)
;; (global-set-key [(meta ?/)] 'hippie-expand)

;; for c/c++
(setq c-basic-offset 4)
;; (add-hook 'c-mode-hook
;;           '(lambda ()
;;              (c-set-style "stroustrup"))) ;code style
(add-hook 'c-mode-hook
          '(lambda()
             (c-toggle-auto-state)))
(add-hook 'c-mode-hook
          '(lambda()
             (c-toggle-auto-newline)))
(add-hook 'c-mode-hook
          '(lambda()
             (c-toggle-auto-hungry-state)))

;; usage:
(require 'ecom-c-style)
(add-hook 'c-mode-common-hook 'ecom-set-c-style)
(add-hook 'c-mode-common-hook 'ecom-make-newline-indent)

;; you may also want to add the following code to your .emacs file
(setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))

;; for color theme
(require 'color-theme)
(color-theme-katester)

