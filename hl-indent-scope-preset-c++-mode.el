;;; hl-indent-scope-preset-c++-mode.el --- C++ preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Preset for C++ mode.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))

(defsubst hl-indent-scope-preset-c++-mode--is-top-level-extern ()
  "Return t when point is part of an `extern' block."
  (declare (important-return-value t))
  (save-excursion
    ;; Move from after the opening brace to the preceding token, allowing
    ;; whitespace and comments between it and the brace.
    (backward-char 1)
    (hl-indent-scope--skip-comments-backward)
    ;; Step over the linkage, the `"C"' of `extern "C" {'.
    (when (hl-indent-scope--skip-string-backward)
      (hl-indent-scope--skip-comments-backward)
      (hl-indent-scope--id-before-point-p "extern"))))

(defsubst hl-indent-scope-preset-c++-mode--is-top-level-namespace ()
  "Return t when point is part of a `namespace' block."
  (declare (important-return-value t))
  (save-excursion
    ;; Move from after the opening brace to the preceding token, allowing
    ;; whitespace and comments between it and the brace.
    (backward-char 1)
    (hl-indent-scope--skip-comments-backward)
    ;; Step back over the name, which may be nested, e.g. `namespace a::b {'.
    ;; NOTE: an attribute between the keyword and the name is not detected,
    ;; e.g. `namespace [[deprecated]] a {'. In practice this is rare enough
    ;; that it doesn't justify scanning back over brackets, so leave as-is.
    (let ((found nil)
          (scan t))
      (while scan
        (setq scan nil)
        (cond
         ;; An anonymous namespace has no name before the brace.
         ((hl-indent-scope--id-before-point-p "namespace")
          (setq found t))
         ((not (zerop (skip-syntax-backward "w_")))
          (hl-indent-scope--skip-comments-backward)
          ;; A nested name may be inline, e.g. `namespace a::inline b {'.
          (when (hl-indent-scope--id-before-point-p "inline")
            (skip-syntax-backward "w_")
            (hl-indent-scope--skip-comments-backward))
          (cond
           ((hl-indent-scope--id-before-point-p "namespace")
            (setq found t))
           ;; Step over `::' to check the component before it.
           ((and (eq (char-before) ?:) (eq (char-before (1- (point))) ?:))
            (backward-char 2)
            (hl-indent-scope--skip-comments-backward)
            (setq scan t))))))
      found)))

(defun hl-indent-scope-preset-c++-mode--show-block-fn (level)
  "Callback for `hl-indent-scope-show-block-fn' at LEVEL."
  (declare (important-return-value t))
  (cond
   ((eq (char-before (point)) ?{)
    (cond
     ((and (zerop level)
           (or (hl-indent-scope-preset-c++-mode--is-top-level-extern)
               (hl-indent-scope-preset-c++-mode--is-top-level-namespace)))
      nil)
     (t
      t)))
   (t
    nil)))


;;;###autoload
(defun hl-indent-scope-preset-c++-mode (&rest args)
  "Preset for `c++-mode' with optional ARGS keyword arguments."
  (declare (important-return-value nil))
  (when args
    (message "Currently ARGS aren't used!"))
  (setq hl-indent-scope-show-block-fn 'hl-indent-scope-preset-c++-mode--show-block-fn))

(provide 'hl-indent-scope-preset-c++-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-preset-c++-mode.el ends here
