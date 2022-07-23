;;; hl-indent-scope-preset-c-mode.el --- C preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope-preset
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:
;; Preset for C mode.

;;; Code:

(eval-when-compile (require 'hl-indent-scope))

(defsubst hl-indent-scope-preset-c-mode--is-top-level-extern ()
  "Return t when POINT is part of an `extern' block."
  (let*
    (
      (found nil)
      (pos (1- (point)))
      (ch (char-before pos)))

    (while (or (eq ch ?\s) (eq ch ?\t))
      (setq pos (1- pos))
      (setq ch (char-before pos)))

    (when (eq ch ?\")
      ;; We have found `" {` which is likely to be an `extern',
      ;; Allow for slower logic here as it's likely to run _mush_ less often,
      ;; than regular (function/conditionals ... etc).
      (let ((str (buffer-substring-no-properties (line-beginning-position) pos)))
        (when (string-match-p "\s*extern\s+\"[[:alpha:]]+\"" str)
          (setq found t))))

    found))

(defun hl-indent-scope-preset-c--show-block-fn (level)
  "Callback for `hl-indent-scope-block-fn' at LEVEL.
Return a tree in range BEG END."
  (cond
    ((eq (char-before (point)) ?{)
      (cond
        ((and (zerop level) (hl-indent-scope-preset-c-mode--is-top-level-extern))
          nil)
        (t
          t)))
    (t
      nil)))


;;;###autoload
(defun hl-indent-scope-preset-c-mode (&rest args)
  "Presets for `c-mode' with optional ARGS keyword arguments."
  (when args
    (message "Currently ARGS isn't used!"))
  (setq hl-indent-scope-show-block-fn 'hl-indent-scope-preset-c--show-block-fn))

(provide 'hl-indent-scope-preset-c-mode)
;;; hl-indent-scope-preset-c-mode.el ends here
