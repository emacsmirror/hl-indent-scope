;;; hl-indent-scope-preset-glsl-mode.el --- GLSL preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope-preset
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Preset for GLSL mode.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))

(defun hl-indent-scope-preset-glsl--show-block-fn (_level)
  "Callback for `hl-indent-scope-block-fn' at LEVEL.
Return a tree in range BEG END."
  (declare (important-return-value t))
  (eq (char-before (point)) ?{))


;;;###autoload
(defun hl-indent-scope-preset-glsl-mode (&rest args)
  "Presets for `c-mode' with optional ARGS keyword arguments."
  (declare (important-return-value nil))
  (when args
    (message "Currently ARGS isn't used!"))
  (setq hl-indent-scope-show-block-fn 'hl-indent-scope-preset-glsl--show-block-fn))

(provide 'hl-indent-scope-preset-glsl-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-preset-glsl-mode.el ends here
