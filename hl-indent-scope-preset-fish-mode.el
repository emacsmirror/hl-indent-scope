;;; hl-indent-scope-preset-fish-mode.el --- Fish shell preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Preset for fish-shell mode.
;;
;; Blocks are keyword delimited, opened by `function', `begin', `while',
;; `switch', `for' & `if' and closed by a generic `end'.  The nesting is
;; handled by `hl-indent-scope-lib-keyword', this file describes the
;; commands & when they count.  A `case' clause is a block of its own,
;; ended by the next clause or by the `end' of its `switch', which is how
;; `fish_indent' lays them out.  The brace compound statement `{ .. }' is
;; the one construct that isn't keyword delimited.
;;
;; Keywords are only keywords in command position, so `echo end' must not
;; close a block.  That test is what most of this file is, the grammar
;; itself is small.
;;
;; Known limitations, all measured against the fish-shell tree & judged
;; too rare to carry the code for:
;;
;; - A keyword written with escapes (`\145n\<newline>d' for `end').
;; - An assignment whose value is a command substitution, as in
;;   `list=a(seq 10) begin'.
;; - Nested quoting, `"$( .. "quoted" .. )"', which the syntax-table
;;   cannot represent, so `syntax-ppss' reports the wrong state.
;; - A bracket or quote after a `#' within a word, as in `(echo a#b)'.
;;   The syntax-table begins a comment at every `#', so `syntax-ppss' misses
;;   them, reporting the wrong state on the lines after.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))
(require 'hl-indent-scope-lib-keyword)

(defconst hl-indent-scope-preset-fish--command-re
  ;; NOTE: `fish-mode' gives `|', `&', `<' & `>' symbol syntax, so `end|sort' is
  ;; one symbol & symbol boundaries miss the keyword.  Keywords begin at a word
  ;; boundary instead & end at a symbol boundary or one of these.  Anything else
  ;; that ends a symbol, as in `end'x'', is rejected by
  ;; `hl-indent-scope-preset-fish--keyword-p'.
  (concat
   "\\<"
   (regexp-opt (list "function" "begin" "while" "switch" "for" "if" "end" "case") t)
   "\\(?:[|&<>]\\|\\_>\\)"
   ;; The brace compound statement, equivalent to `begin' & `end'.
   ;; Note there is no boundary to anchor these to, they are only
   ;; a block when they open a command token, which the predicates test.
   "\\|\\([{}]\\)")
  "Match a block command, a keyword in group 1 or a brace in group 2.")

(defconst hl-indent-scope-preset-fish--decorator-commands
  ;; NOTE: `if' & `while' are here because their condition is a command,
  ;; so `while begin' opens two blocks, which is how `fish_indent' lays it out.
  ;; While `command', `builtin' & `exec' also take a command, it's a plain
  ;; command, `command end' runs a program called `end', so they're excluded.
  (list "and" "or" "not" "!" "time" "if" "while")
  "Commands that take a command, leaving what follows in command position.")

(defconst hl-indent-scope-preset-fish--assignment-re "\\`[[:alnum:]_]+="
  "Match a per-command variable assignment, as in `a=b command'.")

(defun hl-indent-scope-preset-fish--ppss (pos)
  "Return the `syntax-ppss' state at POS, where a `#' within a word is text.
Fish only begins a comment at the beginning of a token, so `echo a#b' prints
`a#b', while `fish-mode' gives every `#' comment syntax."
  (declare (important-return-value t))
  ;; Note that `parse-partial-sexp' moves point.
  (save-excursion
    (let ((state (hl-indent-scope-lib-keyword-ppss pos))
          (start nil)
          ;; Where the parse continues from (after a `#' within a word) & the state there.
          (from nil)
          (from-state nil))
      ;; NOTE: a comment ends with its line, so only a `#' on the line of POS needs checking,
      ;; the lines after one within a word are read as code, see the known limitations
      ;; for the exception.
      (while (and (ppss-comment-depth state)
                  (setq start (ppss-comment-or-string-start state))
                  ;; A `#' after a blank, a command separator, a pipe, the opening parenthesis
                  ;; of a command substitution or a redirection begins a token.
                  (null (memq (char-before start) (list nil ?\n ?\s ?\t ?\; ?| ?\( ?< ?>))))
        ;; The state before the `#', then parse on from after it.
        (setq from-state
              (cond
               (from
                (parse-partial-sexp from start nil nil from-state))
               (t
                (hl-indent-scope-lib-keyword-ppss start))))
        (setq from (1+ start))
        (setq state (parse-partial-sexp from pos nil nil from-state)))
      state)))

(defun hl-indent-scope-preset-fish--code-p (pos)
  "Return non-nil when POS is outside of strings & comments.
As `hl-indent-scope-lib-keyword-code-p', reading a `#' within a word as text,
see `hl-indent-scope-preset-fish--ppss'."
  (declare (important-return-value t))
  (let ((state (hl-indent-scope-preset-fish--ppss pos)))
    (and (null (ppss-string-terminator state)) (null (ppss-comment-depth state)))))

(defun hl-indent-scope-preset-fish--line-continued-p ()
  "Return non-nil when the line before point ends in a line continuation.
Point must be at the beginning of a line."
  (declare (important-return-value t))
  (save-excursion
    (and (null (bobp))
         (let ((count 0))
           ;; Step over the line break, then count the backslashes before it.
           (forward-char -1)
           (while (eq ?\\ (char-before))
             (forward-char -1)
             (setq count (1+ count)))
           (and
            ;; An even number is escaped backslashes, not a continuation.
            (eq 1 (mod count 2))
            ;; NOTE: backslashes are part of a comment & don't join lines,
            ;; see the fish-shell issue #1255.  The count is tested first so
            ;; the state is only needed for the lines that end in one.
            (null (ppss-comment-depth (hl-indent-scope-preset-fish--ppss (point)))))))))

(defun hl-indent-scope-preset-fish--command-position-p (pos)
  "Return non-nil when POS is the beginning of a command.
Fish only treats a keyword as a keyword in command position,
so `echo end' is an argument & must not close a block."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (let ((result nil)
          (searching t)
          ;; The number of commands stepped over to get here.
          (step 0)
          (is-if (looking-at-p "if\\_>")))
      (while searching
        (setq searching nil)
        (skip-chars-backward " \t")
        (cond
         ((bolp)
          (cond
           ;; A continuation makes this the tail of the line above,
           ;; continue from before its backslash.  So after `echo' this is
           ;; an argument, while after a pipe it begins a command.
           ((hl-indent-scope-preset-fish--line-continued-p)
            (forward-char -2)
            (setq searching t))
           (t
            (setq result t))))
         ;; A command separator, a pipe, a job control character or the
         ;; opening parenthesis of a command substitution.
         ;; Note that the two character forms `&&' & `||' end in these too.
         ((memq (char-before) (list ?\; ?| ?& ?\())
          (setq result t))
         ;; The brace of a compound statement, as in `{ if true', unlike the
         ;; brace of an expansion, `echo {if,else}', it's in command position.
         ((eq (char-before) ?{)
          (setq result (hl-indent-scope-preset-fish--command-position-p (1- (point)))))
         (t
          (let ((word-end (point)))
            ;; Step to the beginning of the word before this one.
            ;; Note that the separators end it, `true;and' is two words.
            (skip-chars-backward "^ \t\n;|&({")
            (let ((word (buffer-substring-no-properties (point) word-end)))
              ;; Step over the word & test what comes before it,
              ;; so `and not end' resolves as a command.
              (when (or (member word hl-indent-scope-preset-fish--decorator-commands)
                        ;; NOTE: `else' continues the block above instead of opening
                        ;; one, so the `if' of `else if' must not be taken for a block.
                        ;; What follows that `if' is a command though, as in
                        ;; `else if begin ; true ; end', hence the step count.
                        ;; Any other block after `else' is one, as in `else begin'.
                        (and (string-equal word "else") (or (< 0 step) (null is-if)))
                        (string-match-p
                         hl-indent-scope-preset-fish--assignment-re word))
                (setq step (1+ step))
                (setq searching t)))))))
      result)))

(defun hl-indent-scope-preset-fish--keyword-p (pos)
  "Return non-nil when the word at POS is a keyword in command position.
Only a blank, the end of a line, a line continuation, a command separator,
a redirection or the end of a substitution or compound statement may follow it,
anything else makes it part of a longer word, as in `end_x' or `end=x'."
  (declare (important-return-value t))
  (and (save-excursion
         (goto-char pos)
         (skip-chars-forward "[:alpha:]")
         (or (eolp)
             ;; NOTE: a carriage return is a blank to fish, it's only left
             ;; in the buffer when a file mixes line endings.
             (memq (char-after) (list ?\s ?\t ?\r ?\; ?| ?& ?< ?> ?\) ?}))
             (looking-at-p "\\\\\n")))
       (hl-indent-scope-preset-fish--command-position-p pos)))

(defun hl-indent-scope-preset-fish--brace-close-p (pos)
  "Return non-nil when the brace at POS closes a compound statement.
Alone among the block commands this one may end a command instead of
beginning one, as in `$cmd || { set ok false; break }', so command
position is the wrong test for it.  Instead the brace it pairs with is
tested, a compound statement opens with a brace in command position
while brace expansion, as in `file/{one,two}.txt', doesn't."
  (declare (important-return-value t))
  ;; NOTE: this relies on `fish-mode' giving braces paren syntax, so `syntax-ppss'
  ;; pairs them, including an expansion over lines, as in `{a,\nb}', or one ending
  ;; a token, as in `{ echo a{b,c}}', neither of which the text before the brace
  ;; can answer.
  (let ((state (hl-indent-scope-preset-fish--ppss pos)))
    (and
     ;; An escaped brace is literal text, as in `echo \}'.
     (null (ppss-quoted-p state))
     (let ((open (ppss-innermost-start state)))
       (and open
            (eq ?{ (char-after open))
            (hl-indent-scope-preset-fish--command-position-p open))))))

(defun hl-indent-scope-preset-fish--next-fn (pos limit)
  "Return the first block command at or after POS & before LIMIT.
See `hl-indent-scope-lib-keyword-tree-fn' for the rules this follows."
  (declare (important-return-value t))
  ;; Fish commands are case-sensitive.
  (let ((case-fold-search nil)
        (result nil))
    (goto-char pos)
    (while (and (null result)
                (re-search-forward hl-indent-scope-preset-fish--command-re nil t)
                (< (match-beginning 0) limit))
      ;; NOTE: the match data is read up-front as the syntax check
      ;; may propertize the buffer, which doesn't preserve it.
      (let ((command (or (match-string-no-properties 1) (match-string-no-properties 2)))
            (command-beg (match-beginning 0))
            (command-end (or (match-end 1) (match-end 2))))
        (pcase-let ((`(,type ,kind ,predicate)
                     (pcase command
                       ("end"
                        ;; Generic, it closes whichever block is open.
                        (list 'close nil #'hl-indent-scope-preset-fish--keyword-p))
                       ("case"
                        ;; Both ends the previous clause & begins the next.
                        (list 'clause nil #'hl-indent-scope-preset-fish--keyword-p))
                       ;; NOTE: braces are a different kind of block to keywords, so `}' only
                       ;; closes a block opened by a keyword when it's left open within the
                       ;; braces (likely while it's being typed), ending it there, or the reverse.
                       ;; With one kind for both, `}' would close the keyword block instead,
                       ;; leaving the braces open.
                       ("{" (list 'open 'brace #'hl-indent-scope-preset-fish--command-position-p))
                       ("}" (list 'close 'brace #'hl-indent-scope-preset-fish--brace-close-p))
                       ;; The other keywords open a block.
                       (_ (list 'open nil #'hl-indent-scope-preset-fish--keyword-p)))))
          ;; NOTE: the predicate is called first as it's often cheaper than the
          ;; syntax check & most matches it rejects aren't in a string or comment.
          (when (and (funcall predicate command-beg)
                     (hl-indent-scope-preset-fish--code-p command-beg))
            (setq result (list type command-beg command-end kind))))))
    result))


;;;###autoload
(defun hl-indent-scope-preset-fish-mode (&rest args)
  "Preset for `fish-mode' with optional ARGS keyword arguments."
  (declare (important-return-value nil))
  (when args
    (message "Currently ARGS aren't used!"))
  ;; NOTE: unlike CMake this doesn't use `hl-indent-scope-fixed-width'.
  ;; Fish files are indented 4 wide by `fish_indent' while `tab-width' is 8
  ;; by default, so a fixed width is wrong more often than not.  Detecting
  ;; from the block body gets this right without the user configuring
  ;; anything, & falls back to `tab-width' when there is nothing to detect.
  (setq hl-indent-scope-tree-fn
        (hl-indent-scope-lib-keyword-tree-fn #'hl-indent-scope-preset-fish--next-fn)))

(provide 'hl-indent-scope-preset-fish-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-preset-fish-mode.el ends here
