;;; my-org.el -*- lexical-binding: t; -*-
(defun my-org/org-mode-hook ()
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   )
  )

(defun my-org/org-quick-note ()
  (interactive) (find-file (concat org-directory "scratchpad.org")))

(defun my-org/ledger-capture ()
  (let* ((compstring
          (mapconcat 'identity ledger-expense-completions  "|" ))
         (desc
          (mapconcat 'identity ledger-desc-completions "|" ))
         (f-date
          (format-time-string "%Y/%m/%d" (org-time-string-to-seconds (org-read-date))))
         )
    (format
     capture-expense-template
     f-date
     desc
     compstring
     compstring
     )
    )
  )

(defun my-org/note-capture ()
  (setq derived-secs (org-time-string-to-seconds (org-read-date)))
  (setq my-stamp (format-time-string "%Y/%m/%d" derived-secs))
  (let* (
        (y-m (format-time-string "%Y/%m" derived-secs))
        (prefix (format-time-string "%d" derived-secs))
        (dir (concat org-directory "Journals/" y-m))
        (path (concat dir "/" prefix ".org"))
        )
    (make-directory dir t)
    path
    )
  )

(defun my-org/position-cursor ()
  (goto-char (point-min))
  "")

(defun my-org/org-refile-to-journal ()
  "Refile an entry to journal file's date-tree"
  (interactive)
  (require 'org-datetree)
  (let ((journal (expand-file-name "journal.org" org-directory))
        post-date)
    (setq post-date (or (org-entry-get (point) "TIMESTAMP_IA")
                        (org-entry-get (point) "TIMESTAMP")
                        (org-entry-get (point) "CREATED")))
    (setq post-date (nthcdr 3 (parse-time-string post-date)))
    (setq post-date (list (cadr post-date)
                          (car post-date)
                          (caddr post-date)))
    (org-cut-subtree)
    (with-current-buffer (or (find-buffer-visiting journal)
                             (find-file-noselect journal))
      (save-excursion
        (org-datetree-file-entry-under (current-kill 0) post-date)
        (bookmark-set "org-refile-last-stored")))
    (message "Refiled to %s" journal)
    (org-save-all-org-buffers)
    )
  (setq this-command 'my-org/org-refile-to-journal))

(defun my-org/org-agenda-refile-to-journal ()
  "Refile the item at point to journal."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-remove-subtree-entries-from-agenda)
          (my-org/org-refile-to-journal)))))
  (org-agenda-redo))

;; Use appointment data from org-mode
(defun my-org/agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun my-org/org-display (min-to-app new-time msg)
  (alert (format "Task in %s minutes" min-to-app) :title (format "%s" msg))
  )

(defun my-org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun my-org/confirm-babel-evaluate (lang)
  "Do not ask for confirmation to evaluate code for specified languages."
  (not (member lang '("plantuml" "ditaa" "python" "clojure"))))

(defun my-org/my-window-displaying-agenda-p (window)
  (equal (with-current-buffer (window-buffer window) major-mode)
         'org-mode))

(defun my-org/my-position-calendar-buffer (buffer)
  (let ((agenda-window (car (remove-if-not #'my-org/my-window-displaying-agenda-p (window-list)))))
    (when agenda-window
      (let ((desired-window (split-window agenda-window nil 'below)))
        (set-window-buffer desired-window  buffer)
        desired-window))))

;; (defun my-org/org-journal-find-location ()
;;   ;; Open today's journal, but specify a non-nil prefix argument in order to
;;   ;; inhibit inserting the heading; org-capture will insert the heading.
;;   (org-journal-new-entry t)
;;   ;; Position point on the journal's top-level heading so that org-capture
;;   ;; will add the new entry as a child entry.
;;   (goto-char (point-min))
;;   )

(defun my-org/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which
do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))


(with-eval-after-load 'org-agenda
  (setq
   org-agenda-start-on-weekday nil
   org-agenda-skip-scheduled-if-deadline-is-shown (quote not-today)
   org-agenda-start-day "0d"
   org-agenda-span (quote day)
   org-agenda-timegrid-use-ampm t
   org-agenda-todo-ignore-deadlines 8
   org-agenda-todo-ignore-scheduled (quote future)
   org-deadline-warning-days 7)

  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

  ;; (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)

  (setq org-agenda-files (list
                          (concat org-directory "notes.org")
                          (concat org-directory "gtd.org")
                          (concat org-directory "scratchpad.org")
                          (concat org-directory "journal.org")))


  (setq org-agenda-custom-commands
        '(
          ("n" "Agenda and all TODOs"
           ((agenda "" nil)
            (alltodo "" nil))
           nil)
          ("k" . "Search in notes")
          ("kt" "Note tags search" tags ""
           ((org-agenda-files (file-expand-wildcards (concat org-directory "Journals/*/*/*.org")))))
          ("ks" "Note full text search" search ""
           ((org-agenda-files (file-expand-wildcards (concat org-directory "Journals/*/*/*.org")))))))

  (appt-activate t)

  (setq appt-message-warning-time 15) ; Show notification 15 minutes before event
  (setq appt-display-interval 5)
  ;; Disable multiple reminders
  (setq appt-display-mode-line nil)

  (setq appt-delete-window-function (lambda () t))

  ;; (my-org/agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch

  (run-at-time 5 3600 'my-org/agenda-to-appt) ;; update appt list hourly

  ;; When gtd.org is saved
  ;; (add-hook 'after-save-hook
  ;;           '(lambda ()
  ;;              (if (string= (buffer-file-name) (concat (getenv "HOME") "/Dropbox/Org/gtd.org"))
  ;;                  (my-org/agenda-to-appt))))

  ;; update appt each time agenda opened
  ;; (add-hook 'org-finalize-agenda-hook 'my-org/agenda-to-appt)

  (setq appt-disp-window-function (function my-org/org-display))
  )

(with-eval-after-load 'org-download
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(with-eval-after-load 'org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "757CF06C"))

(with-eval-after-load 'org-id
  ;; Use global IDs
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  )

(with-eval-after-load 'org-babel
  ;; trust certain code as being safe
  (setq org-confirm-babel-evaluate 'my-org/confirm-babel-evaluate)

  ;; automatically show the resulting image
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  )

(with-eval-after-load 'org
  (setq org-directory (expand-file-name "~/Dropbox/Org/"))

  (setq org-refile-targets (quote ((nil :maxlevel . 1)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-startup-indented t)

  (setq org-default-notes-file (concat org-directory "scratchpad.org"))

  (setq
   org-list-allow-alphabetical t
   org-support-shift-select t)

  (setq org-use-sub-superscripts '{})
  (setq org-export-with-sub-superscripts '{})

  (setq org-attach-store-link-p 'attached)
  (if (assoc "\\.pdf\\'" org-file-apps)
      (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . 'emacs) t))

  ;; (add-hook 'org-mode-hook 'turn-off-fci-mode 'append)
  ;; (add-hook 'org-mode-hook 'my/org-mode-hook)

  ;; (setq org-use-speed-commands t)

  ;; Set time when TODO items are done. Don't record anything for one time or repeats
  (setq org-log-done nil)
  (setq org-log-repeat nil)

  ;; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path 'file)

  ;; Save files automatically after a refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))


  (setq ledger-desc-completions
        (list
         "" ;; needed for first | for mapconcat
         "Tmobile" "King Soopers" "Sams #3" "Xcel Energy" "Denver Water" "Comcast"
         "Discover" "United Explorer" "Citibank" "MSI LLC" "US Bank Mortgage"
         "Partners In Pediatrics" "Denver Museum" "Js Noodles" "Target" "Soko Sushi"
         "Woody Creek Cafe" "Vitacost" "United Airlines" "Motorola" "Einstein Bros Bagels"
         "US Bank" "Amazon" "Lidia" "Subway" "Robertson & Sons"
         "Szechuan Chinese Restaurant" "Starbucks" "Harkins Northfield" "Bank Transfer"
         "Williams Family Dentistry" "Costco" "DiscoveryLink" "Netflix" "Top Pho"
         "Bombay Bazaar" "Office Depot" "Myschoolbucks" "Google" "Shaw"
         "UC Digestive Health" "Walgreens" "Mici Italian" "Menchie's" "Red Robin"
         "RTD" "Great Clips" "Home Depot" "Denver Public Works" "Ikea" "Masalaa"
         ))

  (setq ledger-expense-completions
        (list
         "" ;; needed for first | for mapconcat
         "Expenses:Education:Childcare" "Expenses:Homeneeds" "Expenses:Education:Books"
         "Expenses:Bills:TV" "Expenses:Entertainment:Party" "Expenses:Bills:Internet"
         "Expenses:Bills:Phone" "Expenses:Healthcare:Physician"
         "Expenses:School" "Expenses:Car:Maintenance"
         "Expenses:Transit" "Expenses:Education:Tennis" "Expenses:Homeneeds:HOA"
         "Expenses:Homeneeds:HVAC" "Expenses:Education:Charity" "Expenses:Car:Gas"
         "Expenses:Entertainment:Event" "Expenses:Homeneeds:Cleaning"
         "Expenses:Insurance:Life" "Expenses:Bills:Electricity" "Expenses:Bills:Water"
         "Expenses:Food:Restaurants" "Expenses:Food:Grocery"
         "Liabilities:Credit Card:United Explorer" "Expenses:Education:Others"
         "Liabilities:Credit Card:Citibank" "Liabilities:Credit Card:Discover"
         "Assets:FSA:MyCash" "Assets:FSA:Medical" "Assets:FSA:Transit"
         "Assets:Checking:US Bank" "Assets:Savings:US Bank" "Assets:Cash"
         "Income:Salary" "Income:Gift" "Expenses:Homeneeds:Cleaning"
         "Expenses:Insurance:Safety Deposit Box" "Expenses:Education:Meals"
         "Assets:Gift Card:DSST" "Expenses:Homeneeds:Phone" "Expenses:Work:Transit"
         "Expenses:Work:Lodging" "Expenses:Work:Food" "Liabilities:Loans:US Bank"
         "Expenses:Education:Museum" "Expenses:Car:Parking" "Expenses:Healthcare:Medicine"
         "Expenses:Entertainment:Gadgets" "Expenses:Travel:Transit"
         "Expenses:Entertainment:Movies" "Income:Bank Interest"
         "Expenses:Education:Tuition" "Expenses:Healthcare:Dental"
         "Expenses:Entertainment:Gifts" "Income:Reimbursement" "Expenses:Travel:Lodging"
         "Expenses:Education:Music" "Expenses:Education:Supplies"
         "Expenses:Homeneeds:Haircut" "Expenses:Trimarus:Internet" "Expenses:Homeneeds:Cat"
         "Expenses:Homeneeds:Compost" "Expenses:Education:Camp" "Assets:Investments"
         ))

  (setq capture-expense-template
        "%s %%^{Reconciled| |*|!|} %%^{Description%s}
    %%^{Expense%s}  %%? %%^{Amount} USD
    %%^{From%s}")

  ;; Set up templates for org mode capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "gtd.org" "Tasks")
           "* TODO %?\n  Added: %U\n")
          ("s" "Scratchpad" entry (file "scratchpad.org")
           "* %?\n  Added: %^U\n"
           )
          ("j" "Journal Entry"
           entry (file+olp+datetree "journal.org")
           "* %?"
           :time-prompt t
           )
          ("n" "Note"
           plain (file my-org/note-capture)
           ""
           :unnarrowed 1
           :empty-lines 0
           )
          ;; "%(format \"#+STAMP: %s\n\n\" my-stamp)%?")
          ;; ("J" "Journal Entry with Ledger info"
          ;;  entry (file+olp+datetree "journal.org")
          ;;  "* %?\n  #+BEGIN_SRC ledger :noweb yes :tangle finances.dat\n    %<%Y/%m/%d>\n  #+END_SRC"
          ;;  :time-prompt t
          ;;  )
          ("l" "Ledger Entry"
           plain (file "finances.ledger")
           "%(my-org/ledger-capture)"
           :empty-lines 1
           )
          )
        )


  ;; Refile settings
  ;; Exclude DONE state tasks from refile targets

  (setq org-refile-target-verify-function 'my-org/verify-refile-target)

  ;; (add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
  ;;
  ;; (add-hook! org-mode :append
  ;;            #'visual-line-mode
  ;;            #'variable-pitch-mode)

  (add-hook 'org-mode-hook
            '(lambda () (progn
                          (visual-line-mode)
                          ;; (visual-fill-column-mode 1)
                          (my-org/org-mode-hook)
                          (smartparens-mode 0))))

  ;; Needed for tramp execution through babel
  ;; (setq temporary-file-directory "/tmp")

  (setq org-src-fontify-natively t)

  ;; (setq org-refile-targets (quote (
  ;;                                  ("gtd.org" :maxlevel . 9)
  ;;                                  ("notes.org" :maxlevel . 9)
  ;;                                  ("scratchpad.org" :maxlevel . 9)
  ;;                                  )))

  (add-to-list 'display-buffer-alist (cons "\\*Calendar\\*" (cons #'my-org/my-position-calendar-buffer nil)))
  )
