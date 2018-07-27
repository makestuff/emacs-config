;;; oceanic-theme.el --- Oceanic theme.

;; Copyright
;;   (C) 2016 Tengfei Guo <terryr3rd@yeah.net>
;;   (C) 2018 Chris McClelland

;; Author: Tengfei Guo
;; Keywords: oceanic color theme
;; URL: https://github.com/terry3/oceanic-theme
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(deftheme oceanic
  "A color theme based on the Oceanic.")

(custom-theme-set-faces
  'oceanic
  '(default ((t (:foreground "#D8DEE9" :background "#2B2B2B"))))
  '(cursor ((t (:background "#6699CC"))))
  '(fringe ((t (:background "#1a1a1a"))))
  '(region ((t (:background "#343D46"))))
  '(font-lock-builtin-face ((t (:foreground "#FAC863" :weight normal))))
  '(font-lock-comment-face ((t (:foreground "#808080" :weight normal))))
  '(font-lock-function-name-face ((t (:foreground "#D8DE00" :weight normal))))
  '(font-lock-keyword-face ((t (:foreground "#CC7832" :weight bold))))
  '(font-lock-string-face ((t (:foreground "#6A8759" :weight normal))))
  '(font-lock-type-face ((t (:foreground "#CC7832" :weight normal))))
  '(font-lock-constant-face ((t (:foreground "#9876AA" :weight normal))))
  '(font-lock-variable-name-face ((t (:foreground "#A9B7C6" :weight normal))))
  '(minibuffer-prompt ((t (:foreground "#6699CC" :weight normal))))
  '(font-lock-warning-face ((t (:foreground "#EC5F67"  :weight normal))))
  '(highlight ((t (:background "#343D46" :weight normal))))
  '(linum ((t (:foreground "#AB7967" :weight normal))))
  '(mode-line ((t (:background "#3C3F41" :foreground "#9A9A9A" :box "#6699CC" :weight normal))))
  '(mode-line-inactive ((t (:background "#2C2C2C" :foreground "#484848" :box "#6699CC" :weight normal))))
  '(mode-line-highlight ((t (:box nil))))
  '(compilation-error ((t (:background "red" :foreground "#FFFFFF" :weight bold))))
  '(show-paren-match ((t (:background "#3B514D" :foreground "#FFEF28"))))
  '(show-paren-mismatch ((t (:background "#FAC863" :foreground "#FF0000"))))
)

(provide-theme 'oceanic)
