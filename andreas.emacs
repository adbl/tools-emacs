;; Plug-ins -----------------------------------------------------
;; TODO this should actually NOT be done, but runs automatically after .emacs
;; has run
(package-initialize) ;; ELPA + repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; add other plugins to load-path
(add-to-list 'load-path "~/.emacs.d/plugins/")
(let ((default-directory  "~/.emacs.d/plugins/"))
      (normal-top-level-add-subdirs-to-load-path))

;; Pretty emacs -------------------------------------------------

;; no scroll-, tool- or menu-bar, in current and future frames.
(if (string-equal system-type "gnu/linux")
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; color-theme
;(require 'color-theme)
(color-theme-sanityinc-tomorrow-night)

(when (string-equal system-type "gnu/linux")
  (set-face-attribute 'default nil :font "Ubuntu Mono-10:bold"))

(when (string-equal system-type "darwin")
  (set-frame-font "Menlo-14"))

;; transparency!
;(set-frame-parameter nil 'alpha '(100 100))

;; line numbers for each buffer
;; TODO: enable in GUI
(global-linum-mode t)

;; show only left fringe
(set-fringe-mode '(nil . 0))

;; cute arrows indicating buffer boundaries, [not needed with line numbers]
;(setq default-indicate-buffer-boundaries 'none)

;; show column number in modeline
(column-number-mode t)

;; highlight current line
(global-hl-line-mode nil)

;; blinking cursor
(blink-cursor-mode t)

;; make the cursor as wide as the character, e.g. tab character
(setq x-stretch-cursor t)

;; highlight matching parantheses
(show-paren-mode t)
;(setq show-paren-style 'parenthesis)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; no noisy bell
(setq visible-bell 1)

;; no emacs welcome screen
(setq inhibit-startup-message t)

(setq-default show-trailing-whitespace t)

;; Mark background of character at column 80
(require 'column-marker)
(add-hook 'find-file-hook (lambda () (interactive) (column-marker-3 80)))

;; Keyboard -------------------------------------------------

;; shrink/enlarge window vertically
(global-set-key (kbd "C-x C-}") 'enlarge-window)
(global-set-key (kbd "C-x C-{") 'shrink-window)

;; next/previous window in all frames
(global-set-key (kbd "C-x o") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;; clear next line, indent and move down cursor
(global-set-key (kbd "M-RET") (lambda () (interactive)
                                (beginning-of-line)
                                (next-logical-line)
                                (newline-and-indent)
                                (previous-line)
                                (indent-according-to-mode)))

;; comment / uncomment
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

(global-set-key (kbd "C-c r") 'align-regexp)


;; default binding is M-^ which bogs with swedish keyboard
;; (global-set-key (kbd "C-^") 'join-line)

;; default binding is M-{ / M-} which sucks
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;;(global-set-key (kbd "M-/") 'hippie-expand)
;;(global-set-key (kbd "M-,") 'hippie-expand)
;;(global-set-key (kbd "C-,") 'hippie-expand)
;;(global-set-key (kbd "C-'") 'hippie-expand)
(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line))
;                                        try-expand-all-dabbrevs

(global-set-key (kbd "C-c f") 'follow-mode)
(global-set-key (kbd "C-c m") 'magit-status)

;; OS X: normal international behaviour for alt-key
(if (string-equal system-type "darwin")
  (setq ns-alternate-modifier 'none))

;; OS X: use command as meta
(if (string-equal system-type "darwin")
  (setq ns-command-modifier 'meta))

;; OS X: hide, open, fonts
(global-set-key (kbd "ESC M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "ESC M-t") 'ns-popup-font-panel)
(global-set-key (kbd "ESC M-o") 'ns-open-file-using-panel)
(global-set-key (kbd "ESC M-s") 'ns-write-file-using-panel)

;; toggle fullscreen OS X / linux
(if (string-equal system-type "darwin")
  (global-set-key (kbd "ESC M-RET") 'ns-toggle-fullscreen)
  (global-set-key (kbd "ESC M-RET") (lambda ()
    (interactive)
    (set-frame-parameter nil 'fullscreen (if
      (frame-parameter nil 'fullscreen) nil 'fullboth)))))

;; Options --------------------------------------------------

;; FIXME: Makefiles are messed up, tabs are converted to spaces

;; make follow-mode kind of work across multiple frames
(defadvice next-window (before my-next-window-all-frames disable)
  "Enforce the ALL-FRAMES argument to `next-window'."
  (ad-set-arg 2 'visible))
(defadvice follow-all-followers (around my-follow-all-frames activate)
  "Allow `follow-mode' to span frames."
  (ad-enable-advice 'next-window 'before 'my-next-window-all-frames)
  (ad-activate 'next-window)
  ad-do-it
  (ad-disable-advice 'next-window 'before 'my-next-window-all-frames)
  (ad-activate 'next-window))

;; server mode
(when (not (server-mode))
  (server-mode))
;; (setq server-host "10.20.9.170")
;; (setq server-use-tcp t)

;; use tramp-temp-name-prefix to store local copies?
(setq tramp-default-method "ssh"
      tramp-default-host "krabba"
      tramp-default-user "andreas.amsenius")

;; X: use clipboard
(setq-default x-select-enable-clipboard t)

;; OS X: don't open new windows when opening documents externally
(when (string-equal system-type "darwin")
  (setq ns-pop-up-frames nil))

;; automaticly save and restore desktop
(desktop-save-mode 1)

;; indent comments
;;(setq comment-style 'indent)

;; enable downcase/upcase region C-x C-l / C-x C-u
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; enable auto-fill in all major modes
(setq-default auto-fill-function 'do-auto-fill)
;(add-hook 'java-mode-hook 'turn-on-auto-fill)
;(add-hook 'erlang-mode-hook 'turn-on-auto-fill)

;; default fill-column is 80
(setq-default fill-column 80)

;; spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; not needed?
(setq c-basic-offset 4)

;; C-k at beginning of line kills the entire line including newline
(setq kill-whole-line 1)

;; disable backup
(setq backup-inhibited t)

;; use aspell
(setq-default ispell-program-name "aspell")

;; fill-column 68 for text files
(add-hook 'text-mode-hook (lambda () (interactive) (setq fill-column 68)))
;; use flyspell in text-mode
;(add-hook 'text-mode-hook 'turn-on-flyspell)

;; turn on reftex mode for all latex files
(add-hook 'latex-mode-hook 'turn-on-reftex)

;; dont warn when tex-files sets text-main-file to "main.tex"
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((allout-layout . t) (erlang-indent-level . 4) (erlang-indent-level . 2) (tex-main-file . "main.tex")))))

;(require 'helm-config)
;(helm-mode 1)

; ido-mode: magic filename completion
(require 'ido)
(ido-mode t)
; dont switch to window in other frame
(setq ido-default-buffer-method 'selected-window)

;; yasnippets, using <TAB>
(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/snippets/")
;; use x-prompt, then ido-prompt for selecting snippets
(setq yas/prompt-functions '(yas/ido-prompt))
;; also indent first line of snippet according to mode (e.g. method def)
(setq yas/also-auto-indent-first-line 1)
;; snippet expansion inside snippets
(setq yas/triggers-in-field 1)

;; python --------------------
(setq-default python-python-command "ipython")

(require 'python-mode)
(setq py-shell-name "ipython")

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;; erlang --------------------
(when (string-equal system-type "darwin")
  (progn
    (setq erlang-root-dir "/usr/local/lib/erlang/")
    (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))))
(require 'erlang-start)
(setq erlang-indent-level 4)

;; maybe this needs to be before distel in load-path? see wrangler INSTALL
;(require 'wrangler)

;; to be able to compile in tramp
(setq erlang-compile-use-outdir nil)

;; distel
(require 'distel)
(distel-setup)

;; erlang mode for yaws
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;; default nodename for distel (= also for erlang-shell-connect-to-node)
(setq erl-nodename-cache (intern (concat "dev" "@" system-name)))

;; connect to remote node
(defun erlang-shell-connect-to-node ()
  (interactive)
  (let* ((inferior-erlang-machine-options
          (list "-sname" "emacs"
                "-remsh" (format "%s" erl-nodename-cache))))
    (erlang-shell-display)))

(eval-after-load 'erlang
  '(progn
     (defun load-extra-erlang-mode-bindings ()
       (define-key erlang-mode-map (kbd "C-c C-d M") 'erlang-man-function)
       (define-prefix-command 'ctrl-c-ctrl-z-keymap)
       (define-key erlang-mode-map (kbd "C-c C-z") 'ctrl-c-ctrl-z-keymap)
       (define-key ctrl-c-ctrl-z-keymap (kbd "f") 'erlang-shell-with-flags)
       (define-key ctrl-c-ctrl-z-keymap (kbd "z") 'erlang-shell-display)
       (define-key ctrl-c-ctrl-z-keymap (kbd "k") 'erlang-shell-connect-to-node))
     (add-hook 'erlang-mode-hook 'load-extra-erlang-mode-bindings)))

;; This lets you do M-zf to find a (grep)regexp in all kred-sources.
;; Then you can use next error to find the match in the code.
;; E.g. ^C-zf -> Find: OCRGIRO_BNO_START ^C-.
(defun kfind-at (path word)
  (grep-find
   (concat "find " path
           (concat " -name '.svn' -prune -o -name '*~' -prune -o -name '*html' -prune -o -type f -print0 | xargs -0 -e grep -nI -e " word))))
(defun kfind (word)
  (interactive "MFind: ")
  (kfind-at
   (concat
    (car (split-string (buffer-file-name) "lib"))
    ".")
   word)
  )
(setq ctrl-z-map (make-keymap))
(global-set-key (kbd "C-z") ctrl-z-map)
(global-set-key (kbd "C-z f") 'kfind)
(put 'narrow-to-region 'disabled nil)
