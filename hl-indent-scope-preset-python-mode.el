;;; hl-indent-scope-preset-python-mode.el --- Python preset -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope-preset
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:
;; Preset for Python mode.
;;
;; This preset uses much more involved logic as it needs
;; to extract the tree structure from white-space instead
;; of using brackets to define blocks.

;;; Code:

(eval-when-compile
  (require 'hl-indent-scope))

(defconst hl-indent-scope-preset-python--block-commands
  (concat
   "\\_<"
   ;; Commands with arguments:
   "\\(if\\|elif\\|while\\|for\\|def\\|class\\|except\\|with\\|match\\|case\\)"
   ;; Trailing space & parenthesis.
   "\\_>"

   "\\|"
   ;; Commands without arguments:
   "\\(else\\|try\\|finally\\)"
   ;; Trailing space & parenthesis.
   "\\_>"))


;; ---------------------------------------------------------------------------
;; Implement `hl-indent-scope-tree-fn'

(defun hl-indent-scope-preset-python--range-has-indentation (beg end)
  "Return non-nil if text at the start of BEG until END is indented at all."
  (let ((search t)
        (has-indent nil))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (and search (< (point) end))
        (unless (looking-at-p "[[:blank:]]*$")
          (when (memq (char-after (point)) (list ?\s ?\t))
            (setq has-indent t))
          (setq search nil))
        (unless (zerop (forward-line 1))
          (setq search nil))))
    has-indent))

(defun hl-indent-scope-preset-python--expand-back (beg end)
  "Expand BEG backwards as needed (when in the middle of an indentation block).
Argument END is used to limit the search forwards."
  (cond
   ;; Check if the text between beg/end is mid-indentation.
   ((hl-indent-scope-preset-python--range-has-indentation beg end)
    (save-excursion
      ;; Extend the beginning!
      (goto-char beg)
      (let ((search t))
        (while (and search
                    (re-search-backward hl-indent-scope-preset-python--block-commands
                                        (point-min)
                                        t))

          (let ((state (syntax-ppss)))
            ;; Skip strings & comments.
            (unless (or (nth 3 state) (nth 4 state))
              (let ((bol (line-beginning-position)))
                (when (eq (match-beginning 0) bol)
                  (setq search nil)
                  (setq beg bol))))))))
    beg)
   (t
    ;; No indentation, the beginning can be used as-is.
    beg)))


(defun hl-indent-scope-preset-python--calc-block-end-no-args (pos)
  "Given \"else:\" calculate the location of \":\".
This is quite straightforward is it's typically directly after the keyword.
Argument POS is the point at the very end of the command.

See `hl-indent-scope-preset-python--calc-block-end'
note on why there is no limit argument."
  (cond
   ;; Fast path for the common case that will cover nearly all uses of
   ;; else: try: etc... that is where the colon is directly after the command.
   ((eq (char-after pos) ?:)
    (1+ pos))
   (t
    ;; It's possible someone writes "else  :" better support this.
    (save-excursion
      (goto-char pos)
      ;; Worst case, returning this position is not all that bad.
      (let ((pos-result nil)
            (pos-init pos))

        (skip-chars-forward "^:" (point-max))
        (when (eq (char-after (point)) ?:)
          (setq pos-result (1+ (point))))

        (unless pos-result
          (goto-char pos-init)
          (setq pos-result (line-end-position)))
        pos-result)))))

(defun hl-indent-scope-preset-python--calc-block-end (pos)
  "Given \"if a == b:\" calculate the location of \":\".
This becomes more involved when splitting over multiple lines.
Argument POS is the point at the very end of the command.

Note that there is no limit on this bounds for this function as it's
important the proper syntactic end is returned even if this is
out of the begin/end bounds the caller is interested in."
  (save-excursion
    (goto-char pos)
    (let ((pos-result nil) ; Worst case, returning this position is not all that bad.
          (pos-init pos)

          (search t))

      (while (and search (not (zerop (skip-chars-forward "^:" (point-max)))))
        (cond
         ((eq (char-after (point)) ?:)
          (forward-char 1)
          (let ((state (syntax-ppss)))
            ;; Skip strings & comments.
            (unless (or (nth 3 state) (nth 4 state))
              ;; Not a comment, so assume this is the ':' that should be used.
              (setq search nil)
              (setq pos-result (point))
              ;; Check if this point is within brackets.
              (let ((bracket-beg (nth 1 state)))
                ;; Within brackets!
                (when (and bracket-beg (< pos-init bracket-beg))
                  ;; Skip to the next bracket and keep looking.
                  (let ((bracket-end
                         (ignore-errors
                           (scan-sexps bracket-beg 1))))
                    (when bracket-end
                      ;; We managed to find the end of the bracket,
                      ;; move their and keep searching.
                      (goto-char (1- bracket-end))
                      (setq pos-result nil)
                      (setq search t))))))))
         (t
          ;; Use fallback if there is no ':' that can be found.
          (setq search nil))))
      ;; Fallback, in practice this should almost never be used and is most likely to be seen
      ;; when there is temporarily invalid syntax, while weak it's not all that bad.
      (unless pos-result
        (goto-char pos-init)
        (setq pos-result (line-end-position)))
      pos-result)))

(defun hl-indent-scope-preset-python--flat-block-list (beg end)
  "Return all commands between BEG & END.
Commands before BEG may be included depending on expansion."
  (let ((result (list)))
    (save-excursion
      ;; Expand beginning as needed.
      (setq beg (hl-indent-scope-preset-python--expand-back beg end))

      (goto-char end)
      (while (re-search-backward hl-indent-scope-preset-python--block-commands beg t)
        (let ((state (syntax-ppss)))
          ;; Skip strings & comments.
          ;; Also any text inside a nested block used for ternary operators and list comprehension.
          (unless (or (nth 3 state) (nth 4 state) (nth 1 state))
            (let ((match-beg (match-beginning 0))
                  (bol (line-beginning-position)))
              ;; Ensure the command is at the line beginning.
              ;; This excludes "a = b if x else y".
              (when (save-excursion
                      (goto-char bol)
                      (skip-chars-forward "[:blank:]" match-beg)
                      (eq (point) match-beg))
                (let ((cmd-end
                       (cond
                        ;; For commands that take no arguments, use the end of line position.
                        ((match-beginning 2)
                         (hl-indent-scope-preset-python--calc-block-end-no-args (match-end 2)))
                        (t
                         (hl-indent-scope-preset-python--calc-block-end (match-end 1))))))
                  (push (cons (- match-beg bol) (cons match-beg cmd-end)) result))))))))
    result))

(defsubst hl-indent-scope-preset-python--is-comment-at-point (pos)
  "Non-nil when the point at POS is a comment."
  ;; See: Syntax Table Internals.
  ;; Comment start/end or generic comment.
  (memq (car (syntax-after pos)) (list 11 12 14)))

(defsubst hl-indent-scope-preset-python--is-string-at-point-before (pos limit)
  "Non-nil when the point at POS is part of a multi-line string.
Where the string starts before LIMIT."
  (let ((state (syntax-ppss pos)))
    ;; This is a string.
    (when (nth 3 state)
      (let ((string-beg (nth 2 state)))
        ;; Check if the string starts before the limit (the line beginning)
        ;; Making this a multi-line string.
        (and string-beg (< string-beg limit))))))

(defsubst hl-indent-scope-preset-python--is-space-at-point (pos)
  "Non-nil when the point at POS is any white-space."
  (memq (char-after pos) (list ?\n ?\s ?\t)))

(defsubst hl-indent-scope-preset-python--line-indent-is-atleast-or-ignore (ident-limit)
  "Non-nil when line beginning position indentation level is at least IDENT-LIMIT."
  (save-excursion
    ;; (unless (eq (point) (line-beginning-position))
    ;;   (error "Expected BOL"))
    (let* ((bol (point))
           (abs-limit (+ bol ident-limit)))
      (let ((ident-curr (skip-chars-forward "[:blank:]" abs-limit)))
        (cond
         ((eq ident-limit ident-curr)
          ;; At least at limit.
          1)
         ;; White-space.
         ((hl-indent-scope-preset-python--is-space-at-point (point))
          -1)
         ;; Ignore comments.
         ((hl-indent-scope-preset-python--is-comment-at-point (point))
          -1)
         ;; Ignore un-intended parts multi-line strings (that begin before `bol').
         ((hl-indent-scope-preset-python--is-string-at-point-before (point) bol)
          -1)
         (t
          nil))))))

(defun hl-indent-scope-preset-python--skip-indent-level (ident-ofs limit)
  "Move to the next line until a line with indentation less than IDENT-OFS is met.
Limited by LIMIT.
The `(point)' must be at the line beginning."
  (let ((pos (point))
        (changed nil))
    (while (and (< (point) limit)
                (let ((ty
                       (hl-indent-scope-preset-python--line-indent-is-atleast-or-ignore
                        ident-ofs)))
                  (when (eq 1 ty)
                    (setq changed t)
                    (setq pos (point)))
                  ty))
      (unless (zerop (forward-line 1))
        ;; Break.
        (goto-char limit)))

    (goto-char pos)

    ;; Ensure the line is not on white-space.
    (unless changed
      (while (looking-at-p "[[:blank:]]*$")
        (forward-line -1)))

    (line-end-position)))

(defun hl-indent-scope-preset-python--calc-indent-level (ident-ofs)
  "Calculate the indentation level at POINT after.
The result must be greater than IDENT-OFS, otherwise return nil."
  (cond
   ;; Fixed indentation level.
   (hl-indent-scope-fixed-width
    (+ ident-ofs tab-width))
   ;; Calculate the next indent level based on the current line.
   (t
    ;; Calculate indentation,
    ;; Do this by finding the first non-blank, non-comment line
    ;; with an indentation that exceeds `ident-ofs'.
    (let ((search t)
          (ident-ofs-result nil))
      (save-excursion
        (beginning-of-line)
        (while search
          (cond
           ((looking-at-p "[[:blank:]]*$")
            (unless (zerop (forward-line 1))
              (setq search nil)))
           (t
            (let ((ident-curr (skip-chars-forward "[:blank:]" (point-max))))
              (cond
               ((hl-indent-scope-preset-python--is-comment-at-point (point))
                (cond
                 ((zerop (forward-line 1))
                  (beginning-of-line))
                 (t
                  (setq search nil))))
               (t
                ;; Non-comment, non-space character,
                ;; terminate the loop and set the indentation level.
                (setq ident-ofs-result
                      (cond
                       ((< ident-ofs ident-curr)
                        ident-curr)
                       (t
                        ;; The indentation level is below or equal to the current.
                        ;; In this case don't even use the indent level.
                        nil)))
                (setq search nil))))))))
      ident-ofs-result))))

(defun hl-indent-scope-preset-python--tree-fn-impl (beg end flat-keywords ident-current)
  "Return the remaining flat-keywords as well as the resulting tree.
Range BEG END is used to limit the search.
Argument FLAT-KEYWORDS is used to build the tree.
Argument IDENT-CURRENT is the current indentation level being scanned."
  (let ((flat-keywords-next flat-keywords)
        (tree-siblings (list)))
    (while flat-keywords
      (pcase-let ((`(,ident-ofs . (,_cmd-beg . ,cmd-end)) (pop flat-keywords)))
        (cond
         ((< ident-ofs ident-current)
          ;; Break
          (setq flat-keywords nil))
         ((> ident-ofs ident-current)
          (let ((last-sibling (car tree-siblings)))
            (pcase-let ((`(,flat-keywords-child . ,child-result)
                         (hl-indent-scope-preset-python--tree-fn-impl
                          beg
                          end
                          flat-keywords-next
                          ident-ofs)))
              ;; Consume the child tree.
              (setq flat-keywords flat-keywords-child)
              (setcdr last-sibling child-result)
              (when child-result
                (let ((last-sibling-end (cdr (car last-sibling)))
                      (child-end (cdr (car (car child-result)))))

                  (let ((new-end (max last-sibling-end child-end))
                        ;; Limit by the beginning of the next entry or by `end'.
                        (limit
                         (cond
                          (flat-keywords
                           (pcase-let ((`(,_ident-ofs . (,next-cmd-beg . ,_next-cmd-end))
                                        (car flat-keywords)))
                             (min end next-cmd-beg)))
                          (t
                           end))))
                    ;; Extend until de-intend!

                    (goto-char child-end)
                    (beginning-of-line)
                    ;; No need to be on the line the child ends at, skip forward.
                    (forward-line 1)
                    (setq new-end
                          (hl-indent-scope-preset-python--skip-indent-level ident-ofs limit))

                    (setcdr (car last-sibling) new-end))))))

          ;; Step.
          (setq flat-keywords-next flat-keywords))
         (t
          ;; Add sibling.
          (goto-char cmd-end)
          (beginning-of-line)
          (forward-line 1)
          (let ((ident-next (hl-indent-scope-preset-python--calc-indent-level ident-ofs))
                (block-end (point))
                (limit
                 (cond
                  (flat-keywords
                   (pcase-let ((`(,_ident-ofs . (,next-cmd-beg . ,_next-cmd-end))
                                (car flat-keywords)))
                     (min end next-cmd-beg)))
                  (t
                   end))))

            ;; When nil, this is a single line statement.
            ;;    if foo: do(bar)
            ;;    if bar: do(baz)
            ;; These can be skipped entirely but that extracting keyword logic,
            ;; which assumes the last keyword has been added to the list,
            ;; so use the end of line of the keyword instead.
            (setq block-end
                  (cond
                   (ident-next
                    (hl-indent-scope-preset-python--skip-indent-level ident-next limit))
                   (t
                    ;; Single line statement, should happend fairly rarely.
                    (save-excursion
                      (goto-char cmd-end)
                      (line-end-position)))))
            (let ((block-beg cmd-end))
              (push (cons (cons block-beg block-end) nil) tree-siblings)))
          ;; Step.
          (setq flat-keywords-next flat-keywords)))))

    (cons flat-keywords-next tree-siblings)))

(defun hl-indent-scope-preset-python--tree-fn (beg end)
  "Return tree between BEG & END."
  ;;
  (let ((flat-keywords (hl-indent-scope-preset-python--flat-block-list beg end)))
    (let ((tree (cdr (hl-indent-scope-preset-python--tree-fn-impl beg end flat-keywords 0))))
      tree)))


;; ---------------------------------------------------------------------------
;; Public Preset Function

;;;###autoload
(defun hl-indent-scope-preset-python-mode (&rest args)
  "Presets for `python-mode' with optional ARGS keyword arguments."
  (when args
    (message "Currently ARGS isn't used!"))
  (setq hl-indent-scope-tree-fn 'hl-indent-scope-preset-python--tree-fn))

(provide 'hl-indent-scope-preset-python-mode)
;;; hl-indent-scope-preset-python-mode.el ends here
