(prelude-require-packages '(vterm eshell-vterm eshell-up))

(defun pmi/named-term (term-name)
  "Generate a terminal with buffer name TERM-NAME."
  (interactive "sTerminal purpose: ")
  (vterm (concat "term-" term-name)))

(add-hook 'vterm-exit-functions #'(lambda (buf event)
                                    (when buf
                                      (if (= 1 (length (window-list)))
                                          (kill-buffer buf)
                                        (kill-buffer-and-window)))))

(global-set-key (kbd "C-x C-1") 'pmi/named-term)
(global-set-key (kbd "C-x C-2") 'vterm-toggle)
(defalias 'eshell/v 'eshell-exec-visual)
(require 'eshell-up)
(defun eshell/up (dir)
  (eshell-up dir)
  )

(eval-after-load 'vterm
  '(progn
     (eshell-vterm-mode)
     (add-hook 'vterm-mode-hook (lambda ()
                                  (display-line-numbers-mode -1)))

     (define-key vterm-mode-map (kbd "C-x C-4")  'vterm-toggle-forward)
     (define-key vterm-mode-map (kbd "C-x C-5")  'vterm-toggle-backward)))


(add-hook
 'eshell-mode-hook (lambda ()
                     (eshell-cmpl-initialize)
                     (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                     (define-key eshell-mode-map (kbd "M-1 f") 'helm-eshell-prompts-all)
                     (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
