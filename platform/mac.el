;; change command to meta, and ignore option to use weird norwegian keyboard
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; mac friendly font
(set-face-attribute 'default nil :font "Menlo-12")

;; make sure path is correct when launched as application
(setenv "PATH" (concat "/usr/local/share/python:/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)
(push "/usr/local/sbin" exec-path)
(push "/usr/local/share/python" exec-path)

;; keybinding to toggle full screen mode
(global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

(normal-erase-is-backspace-mode t)

;; PeepOpen
;; See https://gist.github.com/1505658 if PeepOpen opens selected files in a new Emacs instance
;(require 'eproject-peepopen)

(provide 'mac)