;; Here are some examples of how to override the defaults for the
;; various prelude-emacs settings.  To *append* to any of the
;; configurations attached to prelude-*-hooks, you can attach a
;; function to the appropriate hook:

;; disable whitespace-mode and whitespace-cleanup
;; (add-hook 'prelude-prog-mode-hook
;;           (lambda ()
;;             (prelude-turn-off-whitespace)
;;             (remove-hook 'before-save-hook 'whitespace-cleanup)) t)

;; For other global settings, just run the appropriate function; all
;; personal/*.el files will be evaluate after prelude-emacs is loaded.

;; disable line highlight
;; (global-hl-line-mode -1)

;; make the cursor blinking
;; (blink-cursor-mode t)

;; install custom packages

(add-to-list 'package-archives
             '("MyMelpa" . "~/repository/melpa/packages/") t)
(package-initialize)

(setq prelude-packages
      (append prelude-packages
              '(csv-mode smex switch-window ace-jump-mode)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; set default directory to $HOME
(setq default-directory "~/")

;; platform dependend configurations
(add-to-list 'load-path "~/.emacs.d/personal/platform")
;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
(when is-mac (require 'mac))

;; Are we on a windows?
(setq is-win (equal system-type 'windows-nt))
(when is-win (require 'win))

;; set cygwin bash as default shell
(setq explicit-bash-args '("--login" "-i"))
(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))

(add-hook 'comint-output-filter-functions
'shell-strip-ctrl-m nil t)

(add-hook 'comint-output-filter-functions
'comint-watch-for-password-prompt nil t)

(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  )

(defun n-shell-simple-send (proc command)
  "17Jan02 - sailor. Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command)
    )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )

;; enable subword-mode that lets you move by camelCase
(global-subword-mode 1)

;; point not keep screen position while scrolling
(setq scroll-preserve-screen-position nil)
(scroll-bar-mode -1)

;; use utf-8 environment as default
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; bs instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-x C-o") 'other-frame)

;; Backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
    kept-new-versions 10
    kept-old-versions 2
    version-control t)

;; smex for executing command
(require 'smex)
(smex-initialize)
(setq smex-save-file "~/.emacs.d/emacs-meta/.smex-items")
(global-set-key (kbd "M-x") 'smex)

;; disable alarm bell and visual bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; ido-mode configuration
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-save-directory-list-file "~/.emacs.d/emacs-meta/.ido.last")

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")


;; disable startup and scratch message
(setq inhibit-startup-message 't)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)

;; indentation and tab stops
(setq-default fill-column '80)
(auto-fill-mode 't)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq-default py-indent-offset 4)

(setq indent-line-function 'insert-tab)
(setq-default require-final-newline nil)

;; nxml
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsd\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
         auto-mode-alist))
(setq nxml-child-indent 4)

;; delete trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; change window switching with switch-mode
(require 'switch-window)

;; change flyspell dictionary with f8
(defun fd-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
    (change (if (string= dic "deutsch8") "english" "deutsch8")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)
      ))

(global-set-key (kbd "<f8>") 'fd-switch-dictionary)

;; ace-jump for easy navigation in the current buffer
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; start emacs server to open other files with this instance of emacs
(server-start)

;; load yas snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/personal/snippets")