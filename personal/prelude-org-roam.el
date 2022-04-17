(prelude-require-packages '(org-roam))

(setq org-roam-directory (expand-file-name "~/SynologyDrive/Roam"))
(setq org-id-locations-file
      (concat (file-name-as-directory org-roam-directory) ".org-id-locations"))
(setq org-roam-db-location (expand-file-name "~/SynologyDrive/Roam/org-roam.db"))
(setq org-roam-dailies-directory "monthly/")

(org-roam-db-autosync-enable)

;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          "** %?"
;;          :if-new (file+head+olp "%<%G-W%V>.org" "#+title: %<%G-W%V>\n"
;;                                 ("%<%A %Y-%m-%d>"))
;;          )))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "** %?"
         :if-new (file+head+olp "%<%Y-%m>.org" "#+title: %<%Y-%m>\n"
                                ("%<%Y-%m-%d>"))
         )))

(defun my-org/roam-search (&optional with-types)
  "Search in `default-directory' with RG.
With WITH-TYPES, ask for file types to search in."
  (interactive "P")
  (mu-helm-rg org-roam-directory with-types))


(defun my-org/get-roam-asset-name ()
  (interactive)
  (let ((fp
         (if (derived-mode-p 'dired-mode)
             (dired-get-file-for-visit)
           (buffer-file-name))
         ))
    fp))

(defun my-org/move-roam-asset ()
  (interactive)
  (let* ((fp (my-org/get-roam-asset-name))
         (tfn (concat
               (make-temp-name (number-to-string (random 1000)))
               "."
               (file-name-extension fp)))
         (target (concat (file-name-as-directory org-roam-directory) "assets/"))
         (tfp (concat (file-name-as-directory target) tfn)))
    (mkdir target t)
    (rename-file fp tfp t)
    (push (list tfp (file-name-nondirectory tfp)) org-stored-links)
    ))

;; Save files automatically after a refile
(advice-add 'org-roam-refile :after 'org-save-all-org-buffers)

(global-set-key (kbd "C-c m l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c m f") 'org-roam-node-find)
(global-set-key (kbd "C-c m i") 'org-roam-node-insert)
(global-set-key (kbd "C-c m t") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c m d") 'org-roam-dailies-capture-date)
(global-set-key (kbd "C-c m r") 'org-roam-refile)
(global-set-key (kbd "C-c m s") 'my-org/roam-search)
(global-set-key (kbd "C-c m m") 'my-org/move-roam-asset)
