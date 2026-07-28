;;; hl-indent-scope-preset-cmake-mode.el --- CMake preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Preset for CMake mode.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))
(require 'hl-indent-scope-lib-keyword)

(defconst hl-indent-scope-preset-cmake--block-commands
  ;; NOTE: each block is a kind of its own, as each close is named after its block,
  ;; so `endfunction' ends a function, along with an `if' left open within it (likely
  ;; while it's being typed), instead of closing the `if' & leaving the function open.
  ;; While CMake only reads `if' & `endif' in the body of a function or macro when it's
  ;; called, these aren't isolated (see `hl-indent-scope-lib-keyword-tree-fn').
  ;; A function is often defined within an `if', where the `endif' should end a function
  ;; left open, while a stray `endif' in a function is a less common mistake.
  (list
   (list "if" 'open 'if)
   (list "function" 'open 'function)
   (list "while" 'open 'while)
   (list "macro" 'open 'macro)
   (list "foreach" 'open 'foreach)
   (list "block" 'open 'block)
   (list "endif" 'close 'if)
   (list "endfunction" 'close 'function)
   (list "endwhile" 'close 'while)
   (list "endmacro" 'close 'macro)
   (list "endforeach" 'close 'foreach)
   (list "endblock" 'close 'block))
  "Block commands, the lower case name, its type & kind.")

(defun hl-indent-scope-preset-cmake--skip-arguments ()
  "Move point over the argument list beginning at point.
When it isn't closed, as while it's being typed, move to the next line
instead, so the rest of the buffer is still read."
  (declare (important-return-value nil))
  (let ((arguments-end
         (condition-case nil
             (scan-lists (point) 1 0)
           (scan-error
            nil))))
    (cond
     (arguments-end
      (goto-char arguments-end))
     (t
      (forward-line 1)))))

(defun hl-indent-scope-preset-cmake--next-fn (pos limit)
  "Return the first block command at or after POS & before LIMIT.
See `hl-indent-scope-lib-keyword-tree-fn' for the rules this follows.
CMake commands only begin lines, so only the beginning of each line is
read, stepping over the argument list of each command, which may span
lines.  So unlike a search, a command name in the arguments of another
command isn't taken for a command."
  (declare (important-return-value t))
  ;; NOTE: `scan-lists' & `forward-comment' apply `syntax-propertize-function' as they
  ;; go (since Emacs 25), which `cmake-mode' uses for bracket arguments & comments,
  ;; as in `#[[ .. ]]', so they're skipped like any other.
  (goto-char pos)
  ;; Every command returned ends after the parenthesis beginning its arguments,
  ;; so unless this is the first call, step over the rest of them first.
  (when (eq ?\( (char-before))
    (forward-char -1)
    (hl-indent-scope-preset-cmake--skip-arguments))
  (let ((result nil))
    (while (and (null result)
                (progn
                  ;; Step over blanks, line breaks & comments.
                  (forward-comment (buffer-size))
                  (and (< (point) limit) (null (eobp)))))
      (let ((name-beg (point)))
        (skip-chars-forward "A-Za-z0-9_")
        (let ((name-end (point)))
          (skip-chars-forward " \t")
          (cond
           ((and (< name-beg name-end) (eq ?\( (char-after)))
            (let ((type-and-kind
                   (cdr
                    (assoc
                     ;; CMake uses case-insensitive commands.
                     (downcase (buffer-substring-no-properties name-beg name-end))
                     hl-indent-scope-preset-cmake--block-commands))))
              (cond
               (type-and-kind
                (forward-char 1)
                (pcase-let ((`(,type ,kind) type-and-kind))
                  (setq result (list type name-beg (point) kind))))
               (t
                (hl-indent-scope-preset-cmake--skip-arguments)))))
           (t
            ;; Not a command, which is invalid CMake, step over the line.
            (forward-line 1))))))
    result))

;;;###autoload
(defun hl-indent-scope-preset-cmake-mode (&rest args)
  "Preset for `cmake-mode' with optional ARGS keyword arguments."
  (declare (important-return-value nil))
  (when args
    (message "Currently ARGS aren't used!"))
  (setq-local hl-indent-scope-fixed-width t)
  (setq hl-indent-scope-tree-fn
        (hl-indent-scope-lib-keyword-tree-fn #'hl-indent-scope-preset-cmake--next-fn)))

(provide 'hl-indent-scope-preset-cmake-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-preset-cmake-mode.el ends here
