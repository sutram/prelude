(crux-with-region-or-line comment-or-uncomment-region)

(setq helm-grep-ag-command (concat "rg"
                                   " --color=always"
                                   " --smart-case"
                                   " --no-heading"
                                   " --line-number %s -- %s %s")
      helm-grep-file-path-style 'relative)

(defun mu-helm-rg (directory &optional with-types)
  "Search in DIRECTORY with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
  (helm-grep-ag-1 (expand-file-name directory)
                  (helm-aif (and with-types
                                 (helm-grep-ag-get-types))
                      (helm-comp-read
                       "RG type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*helm rg types*"))))

(defun mu-helm-project-search (&optional with-types)
  "Search in current project with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg (mu--project-root) with-types))

(defun mu-helm-file-search (&optional with-types)
  "Search in `default-directory' with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg default-directory with-types))

(defun mu--project-root ()
  "Return the project root directory or `helm-current-directory'."
  (require 'helm-ls-git)
  (if-let (dir (helm-ls-git-root-dir))
      dir
    (helm-current-directory)))

(global-set-key (kbd "C-c h g") 'mu-helm-file-search)
