;; usage:
;; (require 'ecom-c-style.el)
;; (add-hook 'c-mode-common-hook 'ecom-set-c-style)
;; (add-hook 'c-mode-common-hook 'ecom-make-newline-indent)
;; 
;; you may also want to add the following code to your .emacs file
;; (setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))
;;

(eval-when-compile (require 'cc-defs))

(defun ecom-c-lineup-expression-plus-4 (langelem)
  (save-excursion
    (back-to-indentation)
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
        (goto-char (match-end 1)))
    (vector (+ 4 (current-column)))))
        
(defconst ecom-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro ecom-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        (member-init-intro . +)
                        (friend -)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . 0)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (innamespace . 0))))
  "Ecom C/C++ Programming Style")

(defun ecom-set-c-style ()
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (c-toggle-hungry-state)
  (setq c-tab-always-indent t)
  (c-add-style "Ecom" ecom-c-style t))

(defun ecom-make-newline-indent ()
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))

(provide 'ecom-c-style)
