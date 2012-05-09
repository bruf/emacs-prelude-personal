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
(setq prelude-packages
      (append prelude-packages
              '(smex)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))


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

;; indentation and tab stops
(setq-default fill-column '80)
(auto-fill-mode 't)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; nxml
(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsd\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
         auto-mode-alist))
(setq nxml-child-indent 4)
