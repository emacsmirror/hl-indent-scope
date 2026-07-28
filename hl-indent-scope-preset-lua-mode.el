;;; hl-indent-scope-preset-lua-mode.el --- Lua preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Preset for Lua mode.
;;
;; Blocks are keyword delimited, opened by `function', `if' & `do' and
;; closed by a generic `end', or opened by `repeat' & closed by `until'.
;; The nesting is handled by `hl-indent-scope-lib-keyword', this file
;; finds the keywords.  An `else' or `elseif' continues its `if', so it's
;; not a block of its own.
;;
;; As the keywords are reserved, a keyword outside of strings & comments
;; is always a keyword, the work here is finding where each body begins,
;; after the parameters of a `function' & the condition of an `if'.
;;
;; Known limitations:
;;
;; - A function's name on the line after `function' is taken for code
;;   missing its parameters, so the body begins after the keyword.
;;   One was found in the Lua files tested against.
;; - `lua-ts-mode' isn't supported, as it doesn't propertize multi-line
;;   strings & comments, so `syntax-ppss' would report the keywords in them.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))
(require 'hl-indent-scope-lib-keyword)

(defconst hl-indent-scope-preset-lua--keyword-re
  ;; NOTE: `while' & `for' aren't included as they always end in `do',
  ;; which opens their block, so the body begins after their condition.
  (concat
   "\\_<"
   (regexp-opt (list "function" "if" "then" "elseif" "else" "do" "repeat" "until" "end") t)
   "\\_>")
  "Match the keywords that delimit blocks & conditions.")

(defconst hl-indent-scope-preset-lua--block-re
  ;; NOTE: `then', `elseif' & `else' are left out as they don't open or close a block,
  ;; matching them only to skip them costs a syntax check each.
  ;; In my tests this is ~20% faster scanning a whole buffer.
  (concat "\\_<" (regexp-opt (list "function" "if" "do" "repeat" "until" "end") t) "\\_>")
  "Match the keywords that open or close blocks.")

(defun hl-indent-scope-preset-lua--parameters-end (pos)
  "Return the end of the parameters after the `function' ending at POS, or nil.
Only names, commas, `...' & comments are read, anything else is either code
missing a parenthesis (likely while it's being typed), or a name over lines,
which in practice isn't written, so the body begins after the `function'."
  (declare (important-return-value t))
  (goto-char pos)
  ;; Step over the name, which is absent for an anonymous function.
  (skip-chars-forward "[:alnum:]_.: \t")
  (when (eq ?\( (char-after))
    (forward-char 1)
    ;; NOTE: comments among the parameters are used for documentation,
    ;; so they're stepped over, while the parenthesis isn't paired using
    ;; `scan-lists' as a missing one would pair with one in the code after.
    (while (progn
             (skip-chars-forward "[:alnum:]_., \t\n")
             (forward-comment 1)))
    (when (eq ?\) (char-after))
      (1+ (point)))))

(defun hl-indent-scope-preset-lua--then-end (pos)
  "Return the end of the `then' after the `if' ending at POS, or nil.
Only a `then' at the same depth as the `if' & outside of any function
in the condition is its own, as in `if f == function() .. end then'."
  (declare (important-return-value t))
  (let ((depth (ppss-depth (hl-indent-scope-lib-keyword-ppss pos)))
        ;; Lua keywords are case-sensitive.
        (case-fold-search nil)
        ;; The blocks open in a function in the condition, zero outside of one.
        (nest 0)
        (result nil)
        (searching t))
    (goto-char pos)
    (while (and searching (re-search-forward hl-indent-scope-preset-lua--keyword-re nil t))
      ;; NOTE: the match data is read up-front as the syntax check
      ;; may propertize the buffer, which doesn't preserve it.
      (let* ((word (match-string-no-properties 1))
             (word-end (match-end 0))
             (state (hl-indent-scope-lib-keyword-ppss (match-beginning 0)))
             (word-depth (ppss-depth state)))
        ;; A keyword at a greater depth is in brackets, skip it.
        (when (and (null (ppss-string-terminator state))
                   (null (ppss-comment-depth state))
                   (<= word-depth depth))
          (cond
           ;; Unlikely, the `if' is in brackets that close before its `then'.
           ((< word-depth depth)
            (setq searching nil))
           ((< 0 nest)
            (pcase word
              ((or "function" "if" "do" "repeat") (setq nest (1+ nest)))
              ((or "end" "until") (setq nest (1- nest)))))
           ((string-equal word "function")
            (setq nest 1))
           (t
            ;; Any other keyword means the `then' is missing (likely while it's being typed),
            ;; so the body begins after the `if'.  Stop instead of taking a later block's `then'.
            (setq searching nil)
            (when (string-equal word "then")
              (setq result word-end)))))))
    result))

(defun hl-indent-scope-preset-lua--next-fn (pos limit)
  "Return the first block keyword at or after POS & before LIMIT.
See `hl-indent-scope-lib-keyword-tree-fn' for the rules this follows.
An `if' ends after its `then' & a `function' after its parameters,
so the next call begins after them."
  (declare (important-return-value t))
  ;; Lua keywords are case-sensitive.
  (let ((case-fold-search nil)
        (result nil))
    (goto-char pos)
    (while (and (null result)
                (re-search-forward hl-indent-scope-preset-lua--block-re nil t)
                (< (match-beginning 0) limit))
      (let ((word (match-string-no-properties 1))
            (word-beg (match-beginning 0))
            (word-end (match-end 0)))
        (when (hl-indent-scope-lib-keyword-code-p word-beg)
          (setq result
                (pcase word
                  ("function"
                   (list
                    'open
                    word-beg
                    (or (hl-indent-scope-preset-lua--parameters-end word-end) word-end)
                    nil))
                  ("if"
                   (list
                    'open
                    word-beg
                    (or (hl-indent-scope-preset-lua--then-end word-end) word-end)
                    nil))
                  ("do" (list 'open word-beg word-end nil))
                  ("end" (list 'close word-beg word-end nil))
                  ;; NOTE: these are a different kind of block to the others, so `until'
                  ;; only closes a block opened by `if' when it's left open within the
                  ;; `repeat' (likely while it's being typed), ending it there, as does
                  ;; `end' closing an `if' around a `repeat' left open.
                  ("repeat" (list 'open word-beg word-end 'repeat))
                  ("until" (list 'close word-beg word-end 'repeat)))))))
    result))

;;;###autoload
(defun hl-indent-scope-preset-lua-mode (&rest args)
  "Preset for `lua-mode' with optional ARGS keyword arguments."
  (declare (important-return-value nil))
  (when args
    (message "Currently ARGS aren't used!"))
  ;; NOTE: unlike CMake this doesn't use `hl-indent-scope-fixed-width'.
  ;; Lua has no prevailing indentation width, while `lua-indent-level' is 4,
  ;; 2 & 3 are common too, so detecting from the block body is more reliable.
  (setq hl-indent-scope-tree-fn
        (hl-indent-scope-lib-keyword-tree-fn #'hl-indent-scope-preset-lua--next-fn)))

(provide 'hl-indent-scope-preset-lua-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-preset-lua-mode.el ends here
