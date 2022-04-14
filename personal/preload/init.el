(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(with-eval-after-load 'flycheck
  ;; (setq-default flycheck-emacs-lisp-initialize-packages t)
  ;; (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (setq flycheck-global-modes '(not emacs-lisp-mode))
)
