;;; hl-indent-scope-preset-makefile-gmake-mode.el --- GNU Make preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Preset for GNU Make mode.
;;
;; Blocks are delimited by directives, the conditionals `ifeq', `ifneq',
;; `ifdef' & `ifndef' closed by `endif', and variable definitions, `define'
;; closed by `endef'.  The nesting is handled by `hl-indent-scope-lib-keyword',
;; this file finds the directives.  An `else' continues its conditional, as
;; in `else ifeq ..', so it's not a block of its own.
;;
;; Known limitations:
;;
;; - The body of a variable definition is read for directives, while make
;;   only reads them when the variable is evaluated, as in `$(eval ..)'.
;;   In practice that's what conditionals in a definition are for,
;;   so they're highlighted as blocks.
;; - A line indented by a tab after a rule is a recipe, not a directive.
;;   Telling the two apart means tracking rules, while recipe lines
;;   beginning with a directive's name are rare, so this isn't checked.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))
(require 'hl-indent-scope-lib-keyword)

(defconst hl-indent-scope-preset-makefile-gmake--directives
  ;; NOTE: definitions are a different kind of block to conditionals, so
  ;; `endif' never closes a definition.  As make doesn't read conditionals
  ;; in a definition, a stray `endif' there would otherwise end it.
  ;; A definition is isolated too, so the `endif' doesn't end a conditional
  ;; around it either, see `hl-indent-scope-lib-keyword-tree-fn'.
  (list
   (list "ifeq" 'open nil)
   (list "ifneq" 'open nil)
   (list "ifdef" 'open nil)
   (list "ifndef" 'open nil)
   (list "endif" 'close nil)
   (list "define" 'open 'define)
   (list "endef" 'close 'define))
  "Block directives, the name, its type & kind.")

(defconst hl-indent-scope-preset-makefile-gmake--define-modifiers
  (list "override" "export" "private")
  "Modifiers that may precede `define', as in `override define'.")

(defun hl-indent-scope-preset-makefile-gmake--end-of-line ()
  "Move point to the end of the line, including the lines joined to it.
A line ending in a backslash is joined to the next, comments included."
  (declare (important-return-value nil))
  (while (progn
           (skip-chars-forward "^\n")
           (and (null (eobp))
                (save-excursion
                  ;; Make removes a carriage return before the line ending first,
                  ;; see `hl-indent-scope-preset-makefile-gmake--word'.
                  (when (eq ?\r (char-before))
                    (forward-char -1))
                  ;; An even number is escaped backslashes, not a continuation.
                  (eq 1 (% (- (skip-chars-backward "\\\\")) 2)))))
    (forward-char 1)))

(defun hl-indent-scope-preset-makefile-gmake--word ()
  "Return the word at point, moving over it, nil when there is no word.
Only a word followed by a blank, a comment or the end of the line is
returned, as make reads `endif:' as a target, not a directive."
  (declare (important-return-value t))
  (let ((word-beg (point)))
    (skip-chars-forward "a-z")
    (and (< word-beg (point))
         ;; NOTE: a carriage return is a line ending which make removes, it's
         ;; only left in the buffer when a file mixes line endings.
         (or (eolp) (memq (char-after) (list ?\s ?\t ?\r ?#)))
         (buffer-substring-no-properties word-beg (point)))))

(defun hl-indent-scope-preset-makefile-gmake--directive ()
  "Return the name of the directive at point as make reads it, or nil.
Point must be at the first word of a line & is moved past the directive.
This doesn't apply to the body of a variable definition,
see `hl-indent-scope-preset-makefile-gmake--body-directive'."
  (declare (important-return-value t))
  (let ((word (hl-indent-scope-preset-makefile-gmake--word))
        (is-modified nil))
    (while (and word (member word hl-indent-scope-preset-makefile-gmake--define-modifiers))
      (skip-chars-forward " \t")
      (setq word (hl-indent-scope-preset-makefile-gmake--word))
      (setq is-modified t))
    (and word
         (assoc word hl-indent-scope-preset-makefile-gmake--directives)
         (or (null is-modified) (equal word "define"))
         ;; A variable may be named after a directive, as in `endif = 1'.
         (progn
           (skip-chars-forward " \t")
           (null (looking-at-p "[:?+!]*=")))
         word)))

(defun hl-indent-scope-preset-makefile-gmake--body-directive ()
  "Return `define' or `endef' at point as make reads them in a definition, or nil.
Point must be at the beginning of a line & is moved past the directive.
Make only reads lines not beginning with a tab (the recipe prefix) for these,
where only a bare `define' nests, as in `define X' but not `override define X'."
  (declare (important-return-value t))
  (and (null (eq ?\t (char-after)))
       (progn
         (skip-chars-forward " \t")
         (looking-at "\\(define\\|endef\\)\\(?:[ \t]\\|\r?$\\)"))
       (progn
         (goto-char (match-end 1))
         (match-string-no-properties 1))))

(defun hl-indent-scope-preset-makefile-gmake--in-definition-p (pos)
  "Return non-nil when POS is in the body of a variable definition."
  (declare (important-return-value t))
  (save-excursion
    (goto-char (point-min))
    (let ((depth 0)
          (line-beg nil))
      (while (progn
               (setq line-beg (point))
               (hl-indent-scope-preset-makefile-gmake--end-of-line)
               (< (point) pos))
        (goto-char line-beg)
        (cond
         ((eq 0 depth)
          (skip-chars-forward " \t")
          (when (equal "define" (hl-indent-scope-preset-makefile-gmake--directive))
            (setq depth 1)))
         (t
          (pcase (hl-indent-scope-preset-makefile-gmake--body-directive)
            ("define" (setq depth (1+ depth)))
            ("endef" (setq depth (1- depth))))))
        (hl-indent-scope-preset-makefile-gmake--end-of-line)
        (forward-line 1))
      (< 0 depth))))

(defun hl-indent-scope-preset-makefile-gmake--next-fn (pos limit)
  "Return the first block directive at or after POS & before LIMIT.
See `hl-indent-scope-lib-keyword-tree-fn' for the rules this follows.
Directives only begin lines, so the first word of each line is read.
A directive ends at the end of its line, so the next call begins there."
  (declare (important-return-value t))
  ;; NOTE: the syntax-table isn't used to skip strings & comments.  Make has
  ;; no strings & a comment can only hide a directive by joining the lines
  ;; after it, which is handled along with any other line.  Using `syntax-ppss'
  ;; would be wrong as well, `makefile-mode' gives quotes string syntax, so the
  ;; apostrophe in `$(info don't)' would begin a string, hiding what follows.
  (goto-char pos)
  (let ((result nil))
    (while (and (null result)
                (progn
                  (skip-chars-forward " \t")
                  (and (< (point) limit) (null (eobp)))))
      (let* ((word-beg (point))
             ;; Note that the search may begin at the end of a directive's line,
             ;; which must not be read again.
             (body-word
              (and (null (eolp))
                   (save-excursion
                     (forward-line 0)
                     (hl-indent-scope-preset-makefile-gmake--body-directive))))
             (word (hl-indent-scope-preset-makefile-gmake--directive)))
        ;; NOTE: make reads `define' & `endef' differently in the body of a definition.
        ;; Where the two readings differ, a scan from the beginning of the buffer tells which
        ;; applies, this is only for lines such as `override define' or a tab before `define',
        ;; which weren't found in the Makefiles tested against, so the cost isn't a concern.
        (setq word
              (cond
               ((equal word body-word)
                word)
               ;; An `endef' only ends a definition, so it's always read as in a body.
               ((or (equal word "endef") (equal body-word "endef"))
                body-word)
               ((or (equal word "define") (equal body-word "define"))
                (cond
                 ((hl-indent-scope-preset-makefile-gmake--in-definition-p word-beg)
                  body-word)
                 (t
                  word)))
               (t
                word)))
        (when word
          (pcase-let ((`(,type ,kind)
                       (cdr (assoc word hl-indent-scope-preset-makefile-gmake--directives))))
            (hl-indent-scope-preset-makefile-gmake--end-of-line)
            (setq result (list type word-beg (point) kind))))
        (unless result
          (hl-indent-scope-preset-makefile-gmake--end-of-line)
          (forward-line 1))))
    result))

;;;###autoload
(defun hl-indent-scope-preset-makefile-gmake-mode (&rest args)
  "Preset for `makefile-gmake-mode' with optional ARGS keyword arguments."
  (declare (important-return-value nil))
  (when args
    (message "Currently ARGS aren't used!"))
  ;; NOTE: unlike CMake this doesn't use `hl-indent-scope-fixed-width'.
  ;; Makefiles are often indented by tabs, where detecting from the block
  ;; body measures a level as one tab instead of `tab-width' spaces.
  (setq hl-indent-scope-tree-fn
        (hl-indent-scope-lib-keyword-tree-fn
         #'hl-indent-scope-preset-makefile-gmake--next-fn
         :isolated-kinds (list 'define))))

(provide 'hl-indent-scope-preset-makefile-gmake-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-preset-makefile-gmake-mode.el ends here
