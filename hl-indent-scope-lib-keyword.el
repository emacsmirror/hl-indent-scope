;;; hl-indent-scope-lib-keyword.el --- Keyword block engine -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Shared tree extraction for keyword delimited languages.
;;
;; A preset supplies a function that finds the next block command, the
;; nesting, clauses & ranges are handled here.  How commands are found is
;; left to the preset, which knows the language.

;;; Code:


;; ---------------------------------------------------------------------------
;; Utilities

(defvar hl-indent-scope-lib-keyword--ppss-last nil
  "The last state from `hl-indent-scope-lib-keyword-ppss', as (POS . STATE).
Bound while the tree is extracted, nil otherwise.")

(defun hl-indent-scope-lib-keyword-ppss (pos)
  "Return the `syntax-ppss' state at POS, without moving point.
While the tree is extracted, the state is parsed on from the last one returned,
so NEXT-FN should use this instead of `syntax-ppss',
see `hl-indent-scope-lib-keyword-tree-fn'."
  (declare (important-return-value t))
  (let ((last hl-indent-scope-lib-keyword--ppss-last)
        (state nil))
    (cond
     ;; NOTE: the scan begins at `point-min' & reads forward, which `syntax-ppss' is slow at,
     ;; as it doesn't move its last position forward when parsing on from it, so each call
     ;; parses from further back.  In my tests this is ~4x faster scanning a Lua buffer.
     ((and last (<= (car last) pos))
      (syntax-propertize pos)
      (with-syntax-table (or syntax-ppss-table (syntax-table))
        ;; Note that `parse-partial-sexp' moves point.
        (setq state (save-excursion (parse-partial-sexp (car last) pos nil nil (cdr last))))))
     (t
      ;; Unlikely, a position before the last, as when a preset looks back.
      ;; Note that `syntax-ppss' moves point when given a position.
      (setq state (save-excursion (syntax-ppss pos)))))
    (when last
      (setcar last pos)
      (setcdr last state))
    state))

(defun hl-indent-scope-lib-keyword-code-p (pos)
  "Return non-nil when POS is outside of strings & comments."
  (declare (important-return-value t))
  (let ((state (hl-indent-scope-lib-keyword-ppss pos)))
    (and (null (ppss-string-terminator state)) (null (ppss-comment-depth state)))))


;; ---------------------------------------------------------------------------
;; Tree Extraction

(defun hl-indent-scope-lib-keyword--next (scan limit)
  "Return the next command from SCAN, see `hl-indent-scope-lib-keyword--tree'.
Argument LIMIT is passed to NEXT-FN."
  (declare (important-return-value t))
  (let ((command (aref scan 2)))
    (cond
     (command
      (aset scan 2 nil))
     (t
      (let ((pos (aref scan 1)))
        (setq command (funcall (aref scan 0) pos limit))
        (when command
          ;; NOTE: the type & shape are checked where they're used, this runs for every
          ;; command so it avoids anything more than reading the bounds.
          (let ((command-beg (nth 1 command))
                (command-end (nth 2 command)))
            ;; Without this a function that doesn't advance would never return.
            (unless (and (<= pos command-beg) (< command-beg limit) (< command-beg command-end))
              (error
               "Hl-indent-scope: command %S must begin at or after %d, before %d & not be empty"
               command pos limit))
            (aset scan 1 command-end))))))
    command))

(defun hl-indent-scope-lib-keyword--error-invalid (command)
  "Signal an error for COMMAND, which has an unknown type or the wrong shape."
  (declare (important-return-value nil))
  (error (concat
          "Hl-indent-scope: invalid command %S, "
          "expected (TYPE BEG END KIND) where TYPE is open, close or clause")
         command))

(defun hl-indent-scope-lib-keyword--push-back (scan command)
  "Hand COMMAND back to SCAN, so the caller's next read returns it."
  (declare (important-return-value nil))
  (aset scan 2 command))

(defun hl-indent-scope-lib-keyword--in-range (beg end nodes)
  "Return the NODES that overlap the range BEG to END."
  (declare (important-return-value t))
  (let ((result nil))
    (dolist (node nodes)
      (pcase-let ((`(,node-beg . ,node-end) (car node)))
        (unless (or (< node-end beg) (< end node-beg))
          (push node result))))
    (nreverse result)))

(defun hl-indent-scope-lib-keyword--tree-impl (beg end scan span-beg kind outer-kinds is-clause)
  "Recursive tree extraction for a block, in the range BEG to END.
Argument SCAN is read with `hl-indent-scope-lib-keyword--next'.
Argument SPAN-BEG is the beginning of the block's body.
Argument KIND is the kind of the block, only a close of the same kind ends it.
Argument OUTER-KINDS is the kinds of the blocks around this one, out to the
nearest isolated block, a close of one of these ends this block, which is left open.
Argument IS-CLAUSE scans the body of a clause,
which is ended by the next clause or by the close of its block,
neither of which it may take for itself.
Return nil for an un-terminated block with no children,
the caller removes blocks out of range."
  (declare (important-return-value t))
  (let ((span-end nil)
        (children nil)
        (command nil))
    ;; A close in the body of an isolated block never ends the blocks around it.
    (when (memq kind (aref scan 3))
      (setq outer-kinds nil))
    ;; The end of a block may be anywhere after it, so search to the end of the buffer.
    (while (and (null span-end)
                (setq command (hl-indent-scope-lib-keyword--next scan (point-max))))
      (pcase command
        (`(open ,_ ,open-end ,open-kind)
         (let ((child
                (hl-indent-scope-lib-keyword--tree-impl
                 beg end scan open-end open-kind (cons kind outer-kinds) nil)))
           (when child
             (push child children))))

        (`(clause ,clause-beg ,clause-end ,_)
         (cond
          (is-clause
           ;; This clause ends here & the next one begins,
           ;; hand it back so the caller begins it.
           (setq span-end clause-beg)
           (hl-indent-scope-lib-keyword--push-back scan command))
          (t
           ;; The clause takes the kind of its block, so the block's close ends it.
           (let ((child
                  (hl-indent-scope-lib-keyword--tree-impl
                   beg end scan clause-end kind outer-kinds t)))
             (when child
               (push child children))))))

        (`(close ,close-beg ,_ ,close-kind)
         (cond
          ((eq kind close-kind)
           (setq span-end close-beg)
           ;; The close ends the block, not the clause,
           ;; hand it back so the caller closes it too.
           (when is-clause
             (hl-indent-scope-lib-keyword--push-back scan command)))
          ;; NOTE: this block was left open (likely while it's being typed) & the close
          ;; is for a block around it.  End this block here & hand the close back,
          ;; so it isn't taken for a stray close, leaving the blocks around this one
          ;; open to the end of the buffer.  A close of no block around it is ignored.
          ((memq close-kind outer-kinds)
           (setq span-end close-beg)
           (hl-indent-scope-lib-keyword--push-back scan command))))

        (_ (hl-indent-scope-lib-keyword--error-invalid command))))

    ;; Un-terminated, use the end of the last child that was found.
    ;; NOTE: children out of range are only removed after this,
    ;; otherwise the block would end before them or be lost entirely.
    (unless span-end
      (setq span-end (cdr (car (car children)))))
    (when span-end
      (cons (cons span-beg span-end) (hl-indent-scope-lib-keyword--in-range beg end children)))))

(defun hl-indent-scope-lib-keyword--tree (beg end next-fn isolated-kinds)
  "Return the tree in the range BEG to END.
Arguments NEXT-FN & ISOLATED-KINDS are as `hl-indent-scope-lib-keyword-tree-fn'."
  (declare (important-return-value t))
  (let ((tree nil)
        ;; The scan state, [NEXT-FN POS PENDING ISOLATED-KINDS], where POS is where NEXT-FN
        ;; continues from & PENDING is a command handed back by
        ;; `hl-indent-scope-lib-keyword--push-back', which is returned first.
        ;; ISOLATED-KINDS isn't read by the scan, it's here as it's needed by every block.
        (scan (vector next-fn (point-min) nil isolated-kinds))
        ;; The parser state, read forward from the beginning of the buffer as the scan is.
        (hl-indent-scope-lib-keyword--ppss-last (cons (point-min) nil))
        (command nil))
    (save-match-data
      ;; Stop searching once end is exceeded.
      (while (and (< (aref scan 1) end)
                  (setq command (hl-indent-scope-lib-keyword--next scan end))
                  ;; A block beginning after the range is out of range, as is every block
                  ;; after it, so stop instead of reading it all only to remove it.
                  (null (and (eq 'open (car command)) (< end (nth 2 command)))))
        ;; Note that a close or clause outside of any block is ignored,
        ;; ending the scan there would skip the rest of the buffer.
        (pcase command
          (`(open ,_ ,open-end ,open-kind)
           (let ((child
                  (hl-indent-scope-lib-keyword--tree-impl
                   beg end scan open-end open-kind nil nil)))
             (when child
               (push child tree))))
          (`(,(or 'close 'clause) . ,_) nil)
          (_ (hl-indent-scope-lib-keyword--error-invalid command)))))
    (hl-indent-scope-lib-keyword--in-range beg end tree)))

(defun hl-indent-scope-lib-keyword-tree-fn (next-fn &rest args)
  "Return a function for `hl-indent-scope-tree-fn'.
Argument NEXT-FN is called with the positions POS & LIMIT, returning the
first block command that begins at or after POS & before LIMIT, or nil when
there are none.  LIMIT is the end of the range when looking for blocks, so
the search may stop there, otherwise the end of the buffer.
A command is (TYPE BEG END KIND), where TYPE is one of:
- `open' begins a block, its body begins at END.
- `close' ends the open block of the same KIND, its body ends at BEG.
  Blocks left open within that block end at BEG too,
  unless an isolated block is between them, see `:isolated-kinds'.
- `clause' ends the previous clause at BEG & begins the next at END,
  as `case' does.  KIND isn't used, the clause takes the kind of its block.
KIND is compared with `eq', nil for languages with one kind of block.

NEXT-FN must follow these rules:
- POS <= BEG < LIMIT & BEG < END, the next call is made with END,
  so commands are returned in order & never overlap.
- Commands in strings & comments must be skipped,
  see `hl-indent-scope-lib-keyword-code-p'.  For the parser state use
  `hl-indent-scope-lib-keyword-ppss', which is faster than `syntax-ppss'.
- It may move point & change the match data, not the buffer text.

Optional ARGS is a property list of:
- `:isolated-kinds', a list of the kinds of block whose body is isolated from
  the blocks around it, a close in the body never ends them.  For languages
  where blocks of another kind aren't read in the body, such as make, which
  doesn't read conditionals in a variable definition."
  (declare (important-return-value t))
  (let ((isolated-kinds nil))
    (while args
      (let ((key (pop args)))
        (unless args
          (error "Hl-indent-scope: missing value for key %S" key))
        (let ((val (pop args)))
          (pcase key
            (:isolated-kinds (setq isolated-kinds val))
            (_ (error "Hl-indent-scope: unknown key %S" key))))))
    (lambda (beg end) (hl-indent-scope-lib-keyword--tree beg end next-fn isolated-kinds))))

(provide 'hl-indent-scope-lib-keyword)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-lib-keyword.el ends here
