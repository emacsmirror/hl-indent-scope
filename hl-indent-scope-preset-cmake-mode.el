;;; hl-indent-scope-preset-cmake-mode.el --- CMake preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope-preset
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:
;; Preset for CMake mode.

;;; Code:

(eval-when-compile (require 'hl-indent-scope))

(defconst hl-indent-scope-preset-cmake--beg-end-commands
  (concat
    "\\_<"
    ;; Begin commands.
    "\\(if\\|function\\|while\\|macro\\|foreach\\)"
    ;; Trailing space & parenthesis.
    "\\_>\s*("

    "\\|"
    ;; End commands.
    "\\(endif\\|endfunction\\|endwhile\\|endmacro\\|endforeach\\)"
    ;; Trailing space & parenthesis.
    "\\_>\s*("))

(defun hl-indent-scope-preset-cmake--tree-impl (beg end use-match)
  "Recursive tree extraction for CMake in range BEG END.
Argument USE-MATCH uses an existing match instead of a new search."
  (let
    (
      (span (cons nil nil))
      (span-beg-fallback nil)
      (children nil))
    (while
      (and
        (null (cdr span))
        (cond
          (use-match
            ;; Only ever use once!
            (setq use-match nil)
            t)
          (t
            (re-search-forward hl-indent-scope-preset-cmake--beg-end-commands (point-max) t))))
      (let ((state (syntax-ppss)))
        ;; Skip strings & comments.
        (unless (or (nth 3 state) (nth 4 state))
          (let*
            (
              (str-open (match-string 1))
              (str-close (and (null str-open) (match-string 2))))

            (cond
              (str-open
                (cond
                  ((null (car span))
                    (setcar span (match-end 1)))
                  (t
                    (let ((child (hl-indent-scope-preset-cmake--tree-impl beg end t)))
                      (when child
                        (unless span-beg-fallback
                          (setq span-beg-fallback (car (car child))))
                        (unless (eq t (cdr child))
                          (push child children)))))))
              (str-close
                ;; Break.
                (setcdr span (match-beginning 2))))))))
    (cond
      ((and (car span) (cdr span))
        (cond
          ((or (< (cdr span) beg) (< end (car span)))
            ;; Return the span so it's possible to know the bounds,
            ;; but this is out of range.
            (cons span t))
          (t
            (cons span children))))
      (children
        (cons (cons span-beg-fallback (cdr (car (car children)))) children))
      (t
        nil))))

(defun hl-indent-scope-preset-cmake--tree-fn (beg end)
  "Callback for `hl-indent-scope-tree-fn'.
Return a tree in range BEG END."
  (let
    (
      (tree nil)
      ;; CMake uses case insensitive commands.
      (case-fold-search t))
    (goto-char (point-min))
    (save-match-data
      (while (< (point) end)
        ;; Stop searching once end is exceeded.
        (let ((child (hl-indent-scope-preset-cmake--tree-impl beg end nil)))
          (cond
            ((null child)
              ;; Exit.
              (goto-char end))
            ((not (eq t (cdr child)))
              ;; Skip t (out of range).
              (push child tree))))))
    tree))


;;;###autoload
(defun hl-indent-scope-preset-cmake-mode (&rest args)
  "Presets for `cmake-mode' with optional ARGS keyword arguments."
  (when args
    (message "Currently ARGS isn't used!"))
  (setq hl-indent-scope-fixed-width t)
  (setq hl-indent-scope-tree-fn 'hl-indent-scope-preset-cmake--tree-fn))

(provide 'hl-indent-scope-preset-cmake-mode)
;;; hl-indent-scope-preset-cmake-mode.el ends here
