;; Basic settings
(set-language-environment "UTF-8")
(setq-default indent-tabs-mode nil)
(setq default-truncate-lines 1)
(setq truncate-partial-width-windows default-truncate-lines)
(setq backup-inhibited 1)
(setq inhibit-startup-message 1)
(setq split-height-threshold nil)
(setq gdb-command-name "m68k-elf-gdb")
(setq default-tab-width 2)

(line-number-mode 1)
(column-number-mode 1)
(auto-save-mode 0)
(menu-bar-mode 0)

;; Minor mode for text to do adaptive indentation (e.g for Twiki, Markdown, LaTeX)
(defun srb-adaptive-indent (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while
    (let (
           (lbp (line-beginning-position))
           (lep (line-end-position))
         )
      (put-text-property lbp lep 'wrap-prefix (fill-context-prefix lbp lep))
      (search-forward "\n" end t)
    )
  )
)
(define-minor-mode srb-adaptive-wrap-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  (save-excursion
    (save-restriction
      (widen)
      (let (
             (buffer-undo-list t)
             (inhibit-read-only t)
             (mod (buffer-modified-p))
           )
        (if srb-adaptive-wrap-mode
          (progn
            (setq word-wrap t)
            (unless (member '(continuation) fringe-indicator-alist)
              (push '(continuation) fringe-indicator-alist)
            )
            (jit-lock-register 'srb-adaptive-indent)
          )
          (jit-lock-unregister 'srb-adaptive-indent)
          (remove-text-properties (point-min) (point-max) '(wrap-prefix pref))
          (setq fringe-indicator-alist
            (delete '(continuation) fringe-indicator-alist))
          (setq word-wrap nil)
        )
        (restore-buffer-modified-p mod)
      )
    )
  )
)

;; Compile commands
(setq compile-command (concat "make -C " (getenv "PWD")))
(setq compilation-read-command nil)
(setq compilation-ask-about-save nil)
(setq compilation-scroll-output 1)

;; Key bindings
(global-set-key [f5] "\C-xo")
(global-set-key [f6] (lambda() (interactive) (split-window-horizontally)))
(global-set-key [f7] "\C-x{")
(global-set-key [f8] 'hexl-mode)
(global-set-key [f9] 'buffer-menu)
(global-set-key [f12] 'compile)

;; Mode customizations
(add-hook 'c-mode-common-hook
  '(lambda()
     (c-set-style "bsd" )
     (setq c-basic-offset 2)
     (c-set-offset 'arglist-close '(c-lineup-close-paren))
  )
)
(add-hook 'asm-mode-hook
  '(lambda()
     (setq indent-tabs-mode t)
     (setq default-tab-width 8)
  )
)
(add-hook 'html-mode-hook
  '(lambda ()
     (setq default-tab-width 2)
  )
)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'srb-adaptive-wrap-mode)

(add-hook 'verilog-mode-hook
  '(lambda()
     (define-key verilog-mode-map (kbd ";") 'self-insert-command)
     (define-key verilog-mode-map (kbd ":") 'self-insert-command)
     (define-key verilog-mode-map (kbd "RET") "\C-j")
     (setq electric-indent-local-mode -1)
  )
)
(custom-set-variables
  '(custom-safe-themes
     (quote
       ("9eacaabd2644c9531f53c1f32c20dfb6bd577482d38c900c9dd5379f27cc2792" "5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" default)))
  '(verilog-auto-indent-on-newlines nil)
  '(verilog-auto-lineup 'ignore)
  '(verilog-case-indent 2)
  '(verilog-cexp-indent 2)
  '(verilog-indent-begin-after-if nil)
  '(verilog-indent-level 2)
  '(verilog-indent-level-behavioral 2)
  '(verilog-indent-level-declaration 2)
  '(verilog-indent-level-directive 2)
  '(verilog-indent-level-module 2)
  '(verilog-indent-lists t)
)

;; Trigger appropriate modes for MakeStuff HDL and CUDA files
(setq auto-mode-alist
  (append
    '(
       ("\\.mhdl\\'" . c-mode)
       ("\\.cu\\'" . c++-mode)
    )
    auto-mode-alist
  )
)

;; Load customizations
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(require 'smooth-scrolling)
(setq smooth-scroll-margin 2)

;; Lisp
(defadvice lisp-indent-line(after my-lisp-indentation)
  "Makes lone close-parens indent like in C"
  (interactive)
  (let ((col-num))
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^\\s-*\)")
        (search-forward "\)")
        (backward-sexp)
        (setf col-num (current-column))
        (forward-sexp)
        (backward-char)
        (delete-region (line-beginning-position) (point))
        (indent-to col-num)
      )
    )
    (if col-num
      (move-to-column col-num)
    )
  )
)
(ad-activate 'lisp-indent-line)
(show-paren-mode 1)
(setq lisp-indent-offset 2)
(defalias 'sh-newline-and-indent 'newline-and-indent)
(defalias 'backward-delete-char-untabify 'backward-delete-char)

;; Theme
(load-theme 'oceanic t)
