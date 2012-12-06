
;;Erlang mode
(add-to-list 'load-path "/opt/local/lib/erlang/lib/tools-2.6.8/emacs")
    (setq erlang-root-dir "/opt/local/lib/erlang")
    (setq exec-path (cons "/opt/local/lib/erlang/bin" exec-path))
    (require 'erlang-start)

;;Flymake + syntaxerl (https://github.com/ten0s/syntaxerl)
(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name temp-file
		(file-name-directory buffer-file-name))))
    (list "/Users/davidalmroth/add-ons/flymake-script/fly-compile"
          (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.erl\\'" flymake-erlang-init))
(defun my-erlang-mode-hook ()
(flymake-mode 1))
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;Disable audio bell when scrolling up and down too far
(setq visible-bell t)

;; Always read file from disk automatically to prevent accidents after git pull
(global-auto-revert-mode t)

;; Enable copy + paste (Windows style)
(cua-mode t)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;;Warn with colors when lines are too long
(require 'whitespace)
(setq whitespace-style
      (quote (face tabs trailing lines-trail lines whitespace-line)))
(custom-set-variables
 '(global-whitespace-mode t)
)

;; I hate tabs!
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;;To make it possible to write [] and {} the normal Mac way on a keyboard
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;;David extra key bindings for Copy and Paste
(global-set-key "\M-c" 'clipboard-kill-ring-save)
(global-set-key "\M-n" 'clipboard-yank)
(global-set-key "\M-b" 'clipboard-yank)


;;Distel
(add-to-list 'load-path "~/add-ons/distel/rost-distel-ab45c21/elisp")
(require 'distel)
(distel-setup)

;; scroll x lines at a time, 3 is nice for Mac
;; (makes emacs vertical scroll less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))

;; default nodename for distel (= also for erlang-shell-connect-to-node)
(defun my-dave ()
  (string-match "[^\.]+" system-name)
  (substring system-name (match-beginning 0) (match-end 0))
  )
(setq erl-nodename-cache (intern (concat "dev" "@" (my-dave))))

;; connect to remote node (block 1)
(defun erlang-shell-connect-to-node ()
  (interactive)
  (let* ((inferior-erlang-machine-options
          (list "-sname" "emacs"
                "-remsh" (format "%s" erl-nodename-cache))))
    (erlang-shell-display)))


