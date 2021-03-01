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

(after! dired
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


;; space T go to Todo file
(defun open-todo-file ()
  "Open todo.org"
  (interactive)
  (find-file "~/MEGA/org/todo.org"))

(map!
 :leader
 :n "T" #'open-todo-file)


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

;; load org configurations
(load-file "~/.doom.d/org-config.el")
