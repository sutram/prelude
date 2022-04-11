(prelude-require-packages '(org-roam))

(setq org-roam-directory (file-truename "~/org-roam"))

(org-roam-db-autosync-enable)

(global-set-key (kbd "C-c m l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c m f") 'org-roam-node-find)
(global-set-key (kbd "C-c m i") 'org-roam-node-insert)
(global-set-key (kbd "C-c m t") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c m d") 'org-roam-dailies-capture-date)
