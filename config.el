;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; name and email
(setq user-full-name "Yasser Tamimi"
      user-mail-address "altamimiy89@gmail.com")

;; font
(setq doom-font (font-spec :family "Source Code Pro" :size 30)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 30)
      doom-big-font (font-spec :family "Source Code Pro" :size 48))


;; directory where projectile will search
(setq projectile-project-search-path '("~/MEGA/dotfiles" "~/MEGA/Programming" "~/MEGA/org"))

;; ranger default file manager
(after! ranger
  (setq ranger-override-dired 'ranger))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;company;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocomplete delay
;; (after! company
;;   '(setq
;;     company-idle-delay 0
;;     company-minimum-prefix-length 1))

;; yasnippet
(yas-global-mode 1)

;; companymode

(add-hook 'after-init-hook 'global-company-mode)

;; snippets
(yas-global-mode 1)

;; company backend group

;; (setq company-idle-delay 0
;;       company-echo-delay 0
;;       company-dabbrev-downcase nil
;;       company-minimum-prefix-length 2
;;       company-selection-wrap-around t
;;       company-transformers '(company-sort-by-occurrence
;;                              company-sort-by-backend-importance))

(setq company-idle-delay 0
      company-echo-delay 0
      company-minimum-prefix-length 1
      company-dabbrev-downcase nil
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;end company;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protect the prompt from being deleted
(setq comint-prompt-read-only t)

;; relative line number
(setq display-line-numbers-type 'visual)

;; send deleted files to trash
(setq-default delete-by-moving-to-trash t)

;; undo after insert mode doesn't take all the insert mode as one block
(setq evil-want-fine-undo t)

;; display time and date in mode-line
(setq display-time-day-and-date t)
(display-time-mode 1)
(setq display-time t)

;; word wraping
                                        ;(+global-word-wrap-mode +1)

;; display battery
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))

;; undo
(setq evil-want-fine-undo t ;; makes insert mode undos genteler
      undo-limit 80000000)

;; windows
(setq evil-split-window-right t
      evil-split-window-below t)

;; treemacs
;;TODO

;;avy
;; attatch to all windows
(use-package! avy
  :init
  '(avy-all-windows t))

;; delete kfjsa trailing white space before saving and add an empty line at the end of the file
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; dim inactive screen
(use-package dimmer
  :custom (dimmer-fraction 0.2)
  :config (dimmer-mode))

;; make lsp a little faster
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; spell checking
;; handle most false positives
(set-flyspell-predicate! '(markdown-mode gfm-mode)
  #'+markdown-flyspell-word-p)

(defun +markdown-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on a link URL, markup, HTML, or references."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'font-lock-comment-face faces)
             (memq 'markdown-code-face faces))
        (not (cl-loop with unsafe-faces = '(markdown-reference-face
                                            markdown-url-face
                                            markdown-markup-face
                                            markdown-comment-face
                                            markdown-html-attr-name-face
                                            markdown-html-attr-value-face
                                            markdown-html-tag-name-face
                                            markdown-code-face)
                      for face in faces
                      if (memq face unsafe-faces)
                      return t)))))

;; pop up with ctrl+;
(require 'flyspell-correct-popup)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

;; enable on source code comments
(require 'flyspell)
(add-hook 'prog-mode-hook
          (lambda ()
            (flyspell-prog-mode)))

;; start up on todo items
(setq initial-buffer-choice "~/MEGA/org/todo.org")


;;;;;;;;;;;;;;;;;;;;;;;;;;;; keymaping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; escape key
(setq
 evil-escape-key-sequence "ii"
 evil-escape-delay 0.2)

(define-key evil-visual-state-map (kbd "ii") 'evil-force-normal-state)


;; remove highlight when pressing enter in normal mode
(define-key evil-normal-state-map (kbd "RET") 'evil-ex-nohighlight)

;; 0 to ^
(define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)

;; j and k move visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; ctrl+arrows to move between windows
(windmove-default-keybindings 'control)

;; ctro+hjkl move between windows
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)


;;;; format with f7
(defun formate-whole-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f7] 'formate-whole-buffer)

;; treemacs with space+-
(map!
 :leader
 :n "-" 'treemacs)

;; ctrl+c e go to config.el
(defun open-config-file ()
  "Open config.el."
  (interactive)
  (find-file "~/.doom.d/config.el"))

;; open the config file with ctrl+c e
(bind-key "C-c e" #'open-config-file)

;; archive shortcut
(defun org-archive-done-tasks ()
  "archive all done"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(map!
 :leader
 :"a" #'org-archive-done-tasks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! org
  (setq org-directory "~/MEGA/org"
        org-agenda-files '("~/MEGA/org")
        org-default-notes-file "~/MEGA/org/notes.org"
        org-hide-emphasis-markers t
        org-log-done 'time
        org-superstar-headline-bullets-list '("◉" "○" "✸")))

;; source code appearance
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; auto save only org mode
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; look
(setq org-ellipsis " ▾")
(setq org-pretty-entities t)

;; capture template
(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Tasks:")
           "* TODO %?\n%i" :append t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Notes:")
           "*  %?\n%i" :append t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :append t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i" :append t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i" :append t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i" :append t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i"
           :heading "Tasks"
           :append nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i"
           :heading "Notes"
           :append t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i"
           :heading "Changelog"
           :append t))))

(setq org-cycle-emulate-tab nil)

;; when over a specila form allow editing but when finished don't show them
(setq org-appear-autoemphasis t
      org-appear-autolinks t
      org-appear-autosubmarkers t)
(add-hook 'org-mode-hook 'org-appear-mode)

;; archive location
(setq org-archive-location "~/MEGA/org/archive/Archive_%s::")

;; show images
(setq org-startup-with-inline-images t)

;; for creating templates
(use-package! doct
  :commands (doct))

;; improve the look of the capture dialog
(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ((equal pressed "ESC") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

;; smaller frame and no modeline for capture window
(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))

(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
        (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
        (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
        (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates (mapcar (lambda (template)
                                   (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                               (spec (plist-get (plist-get props :doct) :icon)))
                                     (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                    "\t"
                                                                    (nth 1 template))))
                                   template)
                                 templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; theme ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq doom-theme 'doom-dracula)
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
