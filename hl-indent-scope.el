;;; hl-indent-scope.el --- Highlight indentation by scope -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Highlight indentation by syntax (or user configurable methods).
;; Currently this works for C-like and Lisp-like languages, with special
;; support for C/C++ & CMake.
;; Tabs are currently not supported.

;;; Usage

;; (hl-indent-scope-mode) ;; activate in the current buffer.

;;; Code:

;; Developer Notes:
;;
;; - It's important never to use `char-syntax' when reading characters,
;;   as the same character may represent different brackets.
;;   (C++ can use <> for angle brackets for as well as operators for e.g.)
;;   Instead read the syntax table from the point e.g. `syntax-after'.

;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup hl-indent-scope nil
  "Highlight indentation based on scope depth.
This is calculated by generating a tree extracted from the syntax-table."
  :group 'faces)

(defcustom hl-indent-scope-idle-delay 0.2175
  "Idle time to wait before highlighting.
Set to 0.0 to highlight immediately (as part of syntax highlighting)."
  :type 'float)

(defcustom hl-indent-scope-fixed-width nil
  "Use fixed width indentation (using `tab-width').
Otherwise detect the indentation from it's contents."
  :type 'boolean)

(defcustom hl-indent-scope-fill-empty-lines nil
  "Display color columns for blank lines."
  :type 'boolean)

(defcustom hl-indent-scope-fill-over-text nil
  "Display colors columns over non white-space characters."
  :type 'boolean)

(defcustom hl-indent-scope-preset t
  "Use the default preset for the major modes (when available).

Otherwise you must configure `hl-indent-scope-show-block-fn' yourself."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Custom Faces

;; Automatically initialized unless already defined.
(defface hl-indent-scope-odd-face (list (list t)) "Face used for odd columns.")
(defface hl-indent-scope-even-face (list (list t)) "Face used for even columns.")


;; ---------------------------------------------------------------------------
;; Custom Callbacks

(defvar-local hl-indent-scope-show-block-fn nil
  "Function that returns non-nil when a block in the syntax-table should be used.

Takes one LEVEL argument which represents the S-expression depth,
taking only used levels into account.

The (point) will be located at the start of the S-expression.
Typically (char-before (point)) can be used to check the kind of bracket.")

(defvar-local hl-indent-scope-indent-block-fn nil
  "Function that returns the indentation level of a block.")

(defvar-local hl-indent-scope-tree-fn nil
  "Function that returns the tree.

This takes two arguments representing the range to return BEG & END.

The resulting list is in the format: ((start . end) children-or-nil)
All items in the lists (including children) should be ordered
from last to first.")

;; ---------------------------------------------------------------------------
;; Internal Variables

;; Keep track of the last overlay, this allows expanding the existing overlay where possible.
;; Useful since font-locking often uses multiple smaller ranges which can be merged into one range.
;; Always check this has not been deleted (has a valid buffer) before use.
(defvar-local hl-indent-scope--idle-overlay-last nil)

(defvar-local hl-indent-scope--idle-timer nil)

;; ---------------------------------------------------------------------------
;; Generic Utility Functions

(defmacro hl-indent-scope--setq-expand-range-to-line-boundaries (pos-beg pos-end)
  "Set POS-BEG the the line beginning, POS-END to the line end."
  (declare (indent 1))
  ;; Ignore field boundaries.
  (let ((inhibit-field-text-motion t))
    `
    (save-excursion
      ;; Extend the ranges to line start/end.
      (goto-char ,pos-end)
      (setq ,pos-end (line-end-position))
      (goto-char ,pos-beg)
      (setq ,pos-beg (line-beginning-position)))))


;; ---------------------------------------------------------------------------
;; Callback Implementations

(defsubst hl-indent-scope--search-forward-open-sexp (end)
  "Search forward syntax table for an opening bracket until END.
This has the same behavior as `search-forward'."
  (skip-syntax-forward "^(" (1- end))
  (cond
    ;; 4 is the code for opening brackets, see:
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Table-Internals.html
    ((eq 4 (car (syntax-after (point))))
      (forward-char 1)
      t)
    (t
      nil)))

(defun hl-indent-scope--next-sexp-by-syntax (end level)
  "Skip forward by syntax, bound by END.
Argument LEVEL is the S-expression depth for `hl-indent-scope-show-block-fn'."
  (let
    ( ;; When nothing is found, don't move the point.
      (found nil)
      (pos-init (point)))
    (cond
      (hl-indent-scope-show-block-fn
        ;; Search until `hl-indent-scope-show-block-fn' succeeds (empty while body).
        (while
          (and
            (hl-indent-scope--search-forward-open-sexp end)
            ;; Keep searching while not found.
            (not
              (when (funcall hl-indent-scope-show-block-fn level)
                (setq found t)
                t)))))
      (t
        (setq found (hl-indent-scope--search-forward-open-sexp end))))
    ;; Keep the point at it's current location unless a new point was found.
    ;; While not essential, it's more difficult to reason about expected behavior
    ;; if a function that fails makes some change to the state.
    (unless found
      (goto-char pos-init))
    found))

(defun hl-indent-scope--top-sexp-by-syntax (beg)
  "Seek BEG backwards to encompass the outer-most s-expression.
If we are not already inside an s-expression, leave all-beg as-is."
  (cond
    (hl-indent-scope-show-block-fn
      (let ((pos-list (list)))
        (let ((beg-next nil))
          (while (setq beg-next (ignore-errors (nth 1 (syntax-ppss beg))))
            (push (setq beg beg-next) pos-list)))
        (save-excursion
          (while pos-list
            (let ((beg-next (pop pos-list)))
              (goto-char (1+ beg-next))
              ;; By definition `level' is always zero here.
              (when (funcall hl-indent-scope-show-block-fn 0)
                (setq beg beg-next)
                ;; Break.
                (setq pos-list nil)))))
        beg))
    (t
      (let ((beg-next nil))
        (while (setq beg-next (ignore-errors (nth 1 (syntax-ppss beg))))
          (setq beg beg-next))
        beg))))

;; ---------------------------------------------------------------------------
;; Shared Functions

(defun hl-indent-scope--overlays-remove (&optional pos-beg pos-end)
  "Remove symbol `hl-indent-scope-mode' overlays from current buffer.
If optional arguments POS-BEG and POS-END exist
remove overlays from range POS-BEG to POS-END
Otherwise remove all overlays."
  (remove-overlays pos-beg pos-end 'hl-indent-scope t))


;; ---------------------------------------------------------------------------
;; Internal Bracket Functions
;;

(defun hl-indent-scope--tree-from-buffer-impl (all-beg all-end _beg end level)
  "Return a tree from the buffer.

The format is ((start . end) children-or-nil)
Arguments ALL-BEG, ALL-END are the full range.
Arguments _BEG END are the range to use.
Argument LEVEL is the S-expression depth for `hl-indent-scope-show-block-fn'."
  (let
    (
      (tree nil)
      (end-bound (min end all-end)))
    (while (and (< (point) end-bound) (hl-indent-scope--next-sexp-by-syntax end-bound level))
      (let ((state (syntax-ppss)))
        ;; Skip strings & comments.
        (unless (or (nth 3 state) (nth 4 state))
          (let ((pos-beg (point)))
            (let ((pos-end (ignore-errors (scan-sexps (1- (point)) 1))))
              (unless pos-end
                (setq pos-end end))
              (unless
                (or
                  ;; Entirely outside the range, ignore.
                  (<= all-end pos-beg) (<= pos-end all-beg)
                  ;; If the S-expression is on one line, there is no need to include it.
                  ;; At least not for the purpose of indentation highlighting.
                  (<= pos-end (line-end-position)))
                (push
                  (cons
                    (cons pos-beg pos-end)
                    (hl-indent-scope--tree-from-buffer-impl
                      all-beg
                      all-end
                      pos-beg
                      pos-end
                      (1+ level)))
                  tree))
              (goto-char pos-end))))))
    tree))

(defun hl-indent-scope--tree-from-buffer (all-beg all-end)
  "Return a tree in range ALL-BEG, ALL-END."
  ;; NOTE: caller must use `save-excursion'.
  (goto-char all-beg)
  (setq all-beg (hl-indent-scope--top-sexp-by-syntax all-beg))
  (hl-indent-scope--tree-from-buffer-impl all-beg all-end all-beg all-end 0))

(defsubst hl-indent-scope--face-from-level (level)
  "Return a face from the indentation LEVEL."
  (cond
    ((zerop (mod level 2))
      'hl-indent-scope-even-face)
    (t
      'hl-indent-scope-odd-face)))

(defun hl-indent-scope--detect-indent (range-beg range-end stop)
  "Detect the next indentation level in (RANGE-BEG RANGE-END).
Argument STOP is the current indentation level, use for reference."
  (cond
    (hl-indent-scope-fixed-width
      (+ stop tab-width))
    (t
      (let
        (
          (stop-next (+ stop tab-width))
          (found nil))
        (save-excursion
          (let ((pos-next nil))
            (goto-char range-beg)
            (setq pos-next (1+ (line-end-position)))
            (while (and (null found) (< pos-next range-end))
              (goto-char pos-next)
              (let ((eol (line-end-position)))
                (let ((skip (skip-syntax-forward " " eol)))
                  (cond
                    ((< stop skip)
                      (setq stop-next skip)
                      (setq found t))
                    (t
                      (setq pos-next (1+ eol)))))))))
        stop-next))))

(defun hl-indent-scope--propertize-stops (stops cache-empty-line-str)
  "It's assumed the point is at the line start.
Argument STOPS are the list of integer large to zero.
Argument CACHE-EMPTY-LINE-STR stores the empty string."
  (let*
    ( ;; It's assumed (point) is at the beginning of the line.
      (pos-bol (point))
      (pos-eol (line-end-position)))
    (cond
      ;; Empty line.
      ((eq pos-bol pos-eol)

        ;; Otherwise do nothing.
        (when hl-indent-scope-fill-empty-lines
          ;; Empty line, add overlay.
          (when (null (car cache-empty-line-str))
            (let ((stops-max (car (car stops))))
              (let ((ov-str (make-string stops-max ?\s)))
                ;; Properties.
                (let ((pos-end (car (pop stops))))
                  (while stops
                    (pcase-let ((`(,pos-beg . ,face) (pop stops)))
                      ;; Unlikely but badly indented files can have stops that go backwards.
                      (when (< pos-beg pos-end)
                        (put-text-property pos-beg pos-end 'font-lock-face face ov-str)
                        (setq pos-end pos-beg)))))
                (setcar cache-empty-line-str ov-str))))

          (let ((ov (make-overlay pos-bol pos-bol)))
            (overlay-put ov 'hl-indent-scope t)
            ;; For some reason sharing strings is NOT working (use `concat').
            (overlay-put ov 'after-string (car cache-empty-line-str)))))
      (t
        (let
          (
            (pos-whitespace
              (cond
                ;; Fill in background over any exiting text.
                (hl-indent-scope-fill-over-text
                  pos-eol)
                ;; Fill in background until white-space ends.
                (t
                  (save-excursion
                    (skip-syntax-forward " " pos-eol)
                    (point))))))
          (unless (eq pos-whitespace pos-bol)
            (let ((pos-end (+ (car (pop stops)) pos-bol)))
              (while stops
                (pcase-let ((`(,pos-beg . ,face) (pop stops)))
                  (setq pos-beg (+ pos-beg pos-bol))
                  (when (< pos-beg pos-whitespace)
                    (setq pos-end (min pos-whitespace pos-end))
                    ;; Unlikely but badly indented files can have stops that go backwards.
                    (when (< pos-beg pos-end)
                      ;; (put-text-property pos-beg pos-end 'font-lock-face face)
                      (let ((ov (make-overlay pos-beg pos-end)))
                        (overlay-put ov 'face face)
                        (overlay-put ov 'evaporate t)
                        (overlay-put ov 'hl-indent-scope t))
                      (setq pos-end pos-beg))))))))))))

(defun hl-indent-scope--font-lock-tree-impl (all-beg all-end tree level stops cache-empty-line-str)
  "Implement full buffer font locking of indentation levels.

Argument TREE is the nested tree to lock.

Argument LEVEL an integer representing the depth of the tree.

Argument STOPS is a list of indentation (width . face) pairs,
ordered largest to smallest, always ending in zero:
e.g: (list (8 . face) (4 . face) (0 . face)).
Arguments ALL-BEG, ALL-END are the full range."
  (while tree
    (pcase-let ((`(,range . ,children) (pop tree)))
      (pcase-let ((`(,range-beg . ,range-end) range))

        ;; Recalculate current indent if this is not top-level.
        (when (and hl-indent-scope-indent-block-fn (cdr stops))
          (let
            ( ;; Optionally calculate a new indentation.
              (indent-for-block
                (save-excursion
                  (goto-char range-beg)
                  (funcall hl-indent-scope-indent-block-fn level))))
            ;; Check the new indentation is different.
            (unless (eq indent-for-block (car (car stops)))
              ;; Replace the first item.
              (setq stops (cons (cons indent-for-block (cdr (car stops))) (cdr stops))))))

        (let*
          (
            (level-next (1+ level))
            (i-next (hl-indent-scope--detect-indent range-beg range-end (car (car stops))))
            (stops-next (cons (cons i-next (hl-indent-scope--face-from-level level-next)) stops))
            (cache-empty-line-str-next (cons nil nil)))

          ;; For zero level indentation there is nothing to do in-between members of the tree.
          (when (zerop level)
            (goto-char range-end)
            (beginning-of-line))

          ;; TODO: avoid forward-line when we're jumping over values out of all{beg/end}
          (cond
            ((zerop level)
              (goto-char range-end)
              (beginning-of-line))
            (t
              (while (<= range-end (point))
                (when (and (< (point) all-end) (<= all-beg (point)))
                  (hl-indent-scope--propertize-stops stops cache-empty-line-str))
                (forward-line -1))))

          (when children
            (hl-indent-scope--font-lock-tree-impl
              all-beg
              all-end
              children
              level-next
              stops-next
              cache-empty-line-str-next))

          (while (<= range-beg (point))
            (when (and (< (point) all-end) (<= all-beg (point)))
              (hl-indent-scope--propertize-stops stops-next cache-empty-line-str-next))
            (forward-line -1)))))))

(defun hl-indent-scope--font-lock-tree (all-beg all-end)
  "Lock tree.
Arguments ALL-BEG, ALL-END are the full range.
This function moves the point, caller may wish to use `save-excursion'."
  (hl-indent-scope--overlays-remove all-beg all-end)
  (save-excursion
    (let
      (
        (tree
          (funcall
            (or hl-indent-scope-tree-fn 'hl-indent-scope--tree-from-buffer) all-beg all-end)))
      (when tree
        ;; Go to the end of the tree's range.
        (goto-char (cdr (car (car tree))))
        (beginning-of-line)

        (hl-indent-scope--font-lock-tree-impl
          all-beg
          all-end
          tree
          0
          (list (cons 0 (hl-indent-scope--face-from-level 0)))
          (cons nil nil))))))

(defun hl-indent-scope--font-lock-fontify-region (pos-beg pos-end)
  "Update highlighting for POS-BEG & POS-END to the queue, checking all text."
  (hl-indent-scope--setq-expand-range-to-line-boundaries
    ;; Warning these values are set in place.
    pos-beg pos-end)
  (hl-indent-scope--font-lock-tree pos-beg pos-end))


;; ---------------------------------------------------------------------------
;; Internal Color Contrast Calculation
;;
;; This logic is used when the face color isn't set.

(defun hl-indent-scope--color-tint (a percent)
  "Tint color A by PERCENT in renga [-100..100]."
  (let ((factor (truncate (* 655.35 percent))))
    (cond
      ((< factor 0)
        (vector
          (max 0 (+ (aref a 0) factor))
          (max 0 (+ (aref a 1) factor))
          (max 0 (+ (aref a 2) factor))))
      (t
        (vector
          (min 65535 (+ (aref a 0) factor))
          (min 65535 (+ (aref a 1) factor))
          (min 65535 (+ (aref a 2) factor)))))))

(defun hl-indent-scope--color-values-as-string (color)
  "Build a color HEX string from COLOR.
Inverse of `color-values'."
  (format "#%02x%02x%02x" (ash (aref color 0) -8) (ash (aref color 1) -8) (ash (aref color 2) -8)))

(defun hl-indent-scope--auto-color-tint-list (tint-list)
  "Return a list of colors, tinted by TINT-LIST which is a list of percentages."
  (let*
    (
      (bg-color (apply #'vector (color-values (face-attribute 'default :background))))
      (is-light-bg (> 98304 (+ (aref bg-color 0) (aref bg-color 1) (aref bg-color 2)))))
    (mapcar
      (cond
        (is-light-bg
          (lambda (i-tint)
            (hl-indent-scope--color-values-as-string
              (hl-indent-scope--color-tint bg-color i-tint))))
        (t
          (lambda (i-tint)
            (hl-indent-scope--color-values-as-string
              (hl-indent-scope--color-tint bg-color (- i-tint))))))
      tint-list)))

(defun hl-indent-scope--auto-color-calc ()
  "Calculation auto colors."
  (pcase-let ((`(,color-lo ,color-hi) (hl-indent-scope--auto-color-tint-list (list 8 16))))
    (custom-set-faces
      (list 'hl-indent-scope-odd-face (list (list t (list :background color-lo))))
      (list 'hl-indent-scope-even-face (list (list t (list :background color-hi)))))))


;; ---------------------------------------------------------------------------
;; Presets

;;;###autoload
(defun hl-indent-scope-preset (&rest args)
  "Load a preset for current mode.
ARGS the first two arguments are positional,
The first is MODE-VALUE to override the current `major-mode'.
The second is QUIET, when non-nil, don't show a message
when the preset isn't found.
The rest are expected to be keyword arguments,
to control the behavior of each preset,
see it's documentation for available keywords."
  (let
    (
      (mode-value nil)
      (quiet nil)
      (args-positional t)
      (args-count 0))

    (while (and args args-positional)
      (let ((arg (car args)))
        (cond
          ((keywordp arg)
            ;; Found a keyword argument, break.
            (setq args-positional nil))
          (t
            (pcase args-count
              (0 (setq mode-value arg))
              (1 (setq quiet arg))
              (_ (error "Only two positional arguments must be given")))
            (setq args-count (1+ args-count))
            (setq args (cdr args))))))

    (unless mode-value
      (setq mode-value (symbol-name major-mode)))
    (let ((preset-sym (intern (concat "hl-indent-scope-preset-" mode-value))))
      (when
        (condition-case err
          (progn
            (require preset-sym)
            t)
          (error
            (unless quiet
              (message "hl-indent-scope: preset %S not found! (%S)" mode-value err))
            nil))
        (apply preset-sym args)
        ;; Signal not ot use automatic fallback.
        t))))


;; ---------------------------------------------------------------------------
;; Immediate Style (hl-indent-scope-idle-delay zero or lower)

(defun hl-indent-scope--immediate-enable ()
  "Enable immediate highlighting."
  ;; Contextual locking is needed since lines need updating when s-expressions are modified.
  (jit-lock-register #'hl-indent-scope--font-lock-fontify-region t))

(defun hl-indent-scope--immediate-disable ()
  "Disable immediate highlighting."
  (jit-lock-unregister #'hl-indent-scope--font-lock-fontify-region)
  (hl-indent-scope--overlays-remove))


;; ---------------------------------------------------------------------------
;; Timer Style (hl-indent-scope-idle-delay over zero)

(defun hl-indent-scope--idle-overlays-remove (&optional pos-beg pos-end)
  "Remove `hl-indent-scope-pending' overlays from current buffer.
If optional arguments POS-BEG and POS-END exist
remove overlays from range POS-BEG to POS-END
Otherwise remove all overlays."
  (remove-overlays pos-beg pos-end 'hl-indent-scope-pending t))

(defun hl-indent-scope--idle-handle-pending-ranges-impl (&optional all-beg all-end)
  "ALL-BEG and ALL-END to clamp the pending regions used.

Although you can pass in specific ranges as needed,
when checking the entire buffer for example."
  (let*
    (
      (clamp-range (and all-beg all-end))
      (overlays-in-view (overlays-in (or all-beg (point-min)) (or all-end (point-max)))))
    (while overlays-in-view
      (let ((item-ov (pop overlays-in-view)))
        (when
          (and
            (overlay-get item-ov 'hl-indent-scope-pending)
            ;; It's possible these become invalid while looping over items.
            (overlay-buffer item-ov))

          (let
            ( ;; Clamped range.
              (pos-beg (overlay-start item-ov))
              (pos-end (overlay-end item-ov)))

            (when clamp-range
              (setq pos-beg (max all-beg pos-beg))
              (setq pos-end (min all-end pos-end)))

            ;; Expand so we don't spell check half a word.
            (hl-indent-scope--setq-expand-range-to-line-boundaries
              ;; Warning these values are set in place.
              pos-beg pos-end)

            (when
              (condition-case err
                ;; Needed so the idle timer won't quit mid-spelling.
                (let ((inhibit-quit nil))
                  (hl-indent-scope--font-lock-tree pos-beg pos-end)
                  t)
                (error
                  (progn
                    ;; Keep since this should be very rare.
                    (message "Early exit 'hl-indent-scope-mode': %s" (error-message-string err))
                    ;; Break out of the loop.
                    (setq overlays-in-view nil)
                    nil)))

              (cond
                (clamp-range
                  (hl-indent-scope--idle-overlays-remove pos-beg pos-end))
                (t
                  (delete-overlay item-ov))))))))))

(defun hl-indent-scope--idle-handle-pending-ranges-timer-callback (buf)
  "Callback that run the repeat timer for buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (bound-and-true-p hl-indent-scope-mode)
        (hl-indent-scope--idle-handle-pending-ranges-impl)
        (when hl-indent-scope--idle-timer
          (cancel-timer hl-indent-scope--idle-timer)))
      (kill-local-variable 'hl-indent-scope--idle-timer))))

(defun hl-indent-scope--idle-font-lock-region-pending (pos-beg pos-end)
  "Track the range to spell check, adding POS-BEG & POS-END to the queue."
  (when
    (and
      hl-indent-scope--idle-overlay-last
      (not (overlay-buffer hl-indent-scope--idle-overlay-last)))
    (setq hl-indent-scope--idle-overlay-last nil))

  (cond
    ;; Extend forwards.
    (
      (and
        hl-indent-scope--idle-overlay-last
        (eq pos-beg (overlay-end hl-indent-scope--idle-overlay-last)))
      (move-overlay
        hl-indent-scope--idle-overlay-last
        (overlay-start hl-indent-scope--idle-overlay-last)
        pos-end))
    ;; Extend backwards.
    (
      (and
        hl-indent-scope--idle-overlay-last
        (eq pos-end (overlay-start hl-indent-scope--idle-overlay-last)))
      (move-overlay
        hl-indent-scope--idle-overlay-last
        pos-beg
        (overlay-end hl-indent-scope--idle-overlay-last)))
    (t
      (let ((item-ov (make-overlay pos-beg pos-end)))
        ;; Handy for debugging pending regions to be checked.
        ;; (overlay-put item-ov 'face '(:background "#000000" :extend t))

        (overlay-put item-ov 'hl-indent-scope-pending t)
        (overlay-put item-ov 'evaporate 't)

        (setq hl-indent-scope--idle-overlay-last item-ov))))

  ;; Use `inhibit-quit' as a way to check if `jit-lock-stealth' is in use.
  (cond
    (inhibit-quit
      (hl-indent-scope--idle-handle-pending-ranges-impl pos-beg pos-end))
    (t
      ;; Ideally we would just use the existing timer, but there is a very low chance
      ;; the timer has already run (if for some reason
      ;; `hl-indent-scope--idle-handle-pending-ranges-timer-callback' is canceled for e.g.
      ;; So use repeat and the timer can cancel it's self.
      (unless hl-indent-scope--idle-timer
        (setq hl-indent-scope--idle-timer
          (run-with-idle-timer
            hl-indent-scope-idle-delay
            ;; Not actually a repeat timer,
            ;; just done so we can be sure this runs until the callback disables the timer.
            ;; This ensures any interruption won't leave an incomplete state.
            :repeat #'hl-indent-scope--idle-handle-pending-ranges-timer-callback (current-buffer)))))))

(defun hl-indent-scope--idle-enable ()
  "Enable the idle style of updating."
  ;; Unlike with immediate style, idle / deferred checking isn't as likely to
  ;; run before fonts have been checked.
  ;; Nevertheless, this avoids the possibility of spell checking
  ;; running before font-faces have been set.
  (jit-lock-register #'hl-indent-scope--idle-font-lock-region-pending))

(defun hl-indent-scope--idle-disable ()
  "Disable the idle style of updating."
  (jit-lock-unregister #'hl-indent-scope--idle-font-lock-region-pending)
  (hl-indent-scope--overlays-remove)
  (hl-indent-scope--idle-overlays-remove)
  (kill-local-variable 'hl-indent-scope--idle-overlay-last)
  (kill-local-variable 'hl-indent-scope--idle-timer))


;; ---------------------------------------------------------------------------
;; Internal Mode Management

(defun hl-indent-scope--mode-enable ()
  "Turn on `hl-indent-scope-mode' for the current buffer."
  ;; Batch mode, colors don't make sense (for testing).
  (unless noninteractive
    (when
      (or
        (eq 'unspecified (face-attribute 'hl-indent-scope-odd-face :background))
        (eq 'unspecified (face-attribute 'hl-indent-scope-even-face :background)))
      (hl-indent-scope--auto-color-calc)))

  (when hl-indent-scope-preset
    (cond
      ((hl-indent-scope-preset nil t)
        ;; The preset was found & loaded, no further work needed.
        nil)
      ;; When the language uses curly brackets, assume C family or similar.
      ((eq (char-syntax ?{) ?\()
        (setq hl-indent-scope-show-block-fn (lambda (_level) (eq (char-before (point)) ?{))))
      ;; Otherwise assume it's a Lisp.
      (t
        (setq hl-indent-scope-show-block-fn (lambda (_level) (eq (char-before (point)) ?\()))
        ;; Needed so each S-expression can have different indentation.
        (setq hl-indent-scope-indent-block-fn
          (lambda (_level) (max 0 (1- (- (point) (line-beginning-position)))))))))

  (cond
    ((<= hl-indent-scope-idle-delay 0.0)
      (hl-indent-scope--immediate-enable))
    (t
      (hl-indent-scope--idle-enable))))

(defun hl-indent-scope--mode-disable ()
  "Turn off `hl-indent-scope-mode' for the current buffer."

  (remove-overlays (point-min) (point-max) 'hl-indent-scope t)

  (kill-local-variable 'hl-indent-scope-show-block-fn)

  (cond
    ((<= hl-indent-scope-idle-delay 0.0)
      (hl-indent-scope--immediate-disable))
    (t
      (hl-indent-scope--idle-disable))))

(defun hl-indent-scope--mode-turn-on ()
  "Enable command `hl-indent-scope-mode'."
  (when (and (not (minibufferp)) (not (bound-and-true-p hl-indent-scope-mode)))
    (hl-indent-scope-mode 1)))


;; ---------------------------------------------------------------------------
;; Public API

;;;###autoload
(defun hl-indent-scope-buffer ()
  "One off syntax highlighting of indentation, use for testing."
  (interactive)
  (save-excursion (hl-indent-scope--font-lock-tree (point-min) (point-max))))

;;;###autoload
(define-minor-mode hl-indent-scope-mode
  "Highlight block under the cursor."
  :global nil
  (cond
    (hl-indent-scope-mode
      (hl-indent-scope--mode-enable))
    (t
      (hl-indent-scope--mode-disable))))

;;;###autoload
(define-globalized-minor-mode
  global-hl-indent-scope-mode

  hl-indent-scope-mode hl-indent-scope--mode-turn-on)

(provide 'hl-indent-scope)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope.el ends here
