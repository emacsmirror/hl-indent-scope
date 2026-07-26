;;; hl-indent-scope-test.el --- Highlight indent scope test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is a test for `hl-indent-scope'.
;;

;;; Usage

;;
;; To test this file run:
;;
;;     `emacs -batch -l tests/hl-indent-scope-test.el -f ert-run-tests-batch-and-exit'
;;

;;; Code:

(require 'ert)

;; ---------------------------------------------------------------------------
;; Setup Environment

(setq hl-indent-scope-basedir (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path hl-indent-scope-basedir)
(require 'hl-indent-scope)


;; ---------------------------------------------------------------------------
;; Test Utilities

(defun hl-indent-scope-test--clean-buffer (char-odd char-even)
  "Replace CHAR-ODD & CHAR-EVEN with spaces."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat
                               "\\("
                               (regexp-quote (char-to-string char-odd))
                               "\\|"
                               (regexp-quote (char-to-string char-even))
                               "\\)")
                              nil t)
      (replace-match " "))))

(defun hl-indent-scope-test--from-overlays ()
  "Return the overlay ranges found in the buffer."
  (let ((result (list)))
    (let ((ov-list (overlays-in (point-min) (point-max))))
      (while ov-list
        (let ((ov (pop ov-list)))
          (when (overlay-get ov 'hl-indent-scope)
            (let ((ov-face (overlay-get ov 'face)))
              (push (list
                     (overlay-start ov) (overlay-end ov)
                     (cond
                      ((eq ov-face 'hl-indent-scope-odd-face)
                       1)
                      ((eq ov-face 'hl-indent-scope-even-face)
                       2)
                      (t
                       (error "Unknown face %S!" ov-face))))
                    result))))))
    ;; Sort by the first element.
    (sort result (lambda (a b) (< (car a) (car b))))))

(defun hl-indent-scope-test--pos-from-line (line)
  "Return the position at the beginning of LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun hl-indent-scope-test--do-test-on-current-buffer
    (char-odd char-even &optional line-beg line-end)
  "Run a test on the current buffer using CHAR-ODD & CHAR-EVEN.
Optional LINE-BEG & LINE-END limit the range to those lines,
as font-locking a chunk of the buffer does."

  (hl-indent-scope-test--clean-buffer char-odd char-even)

  ;; Enable the mode so presets are loaded.
  (hl-indent-scope-mode)

  (cond
   ((or line-beg line-end)
    ;; Note that the range is expanded to line boundaries by the callee,
    ;; so LINE-END is included even though this is its beginning.
    (hl-indent-scope--font-lock-fontify-region
     (cond
      (line-beg
       (hl-indent-scope-test--pos-from-line line-beg))
      (t
       (point-min)))
     (cond
      (line-end
       (hl-indent-scope-test--pos-from-line line-end))
      (t
       (point-max)))))
   (t
    (hl-indent-scope-buffer)))

  (dolist (ov-info (hl-indent-scope-test--from-overlays))
    (pcase-let ((`(,beg ,end ,is-odd) ov-info))
      (delete-region beg end)
      (goto-char beg)
      (insert
       (make-string
        (- end beg)
        (cond
         ((eq 1 is-odd)
          char-odd)
         ((eq 2 is-odd)
          char-even)
         (t
          (error "Invalid value")))))))
  (buffer-substring-no-properties (point-min) (point-max)))

;; ---------------------------------------------------------------------------
;; Tests

(ert-deftest c-complex ()
  "Complex C test."
  (let ((buf (generate-new-buffer "untitled.c")))
    (with-current-buffer buf
      (c-mode)
      (setq-local tab-width 2)

      (insert
       "/* This is a comment {}. */\n"
       "#include \"test{}.h\"\n"
       "\n"
       "int main(void)\n"
       "{\n"
       "@@if (foo) {\n"
       "@@$$this_is_foo();\n"
       "@@$$that_is_foo();\n"
       "@@}\n"
       "@@else {\n"
       "@@$$testme();\n"
       "@@$$if (bar) {\n"
       "@@$$@@baz();\n"
       "@@$$@@tas();\n"
       "@@$$}\n"
       "@@}\n"
       "@@return 1;"
       "\n}\n"
       "\n"
       "static void test_me(int a)\n"
       "{\n"
       "@@foo_bar();\n"
       "\n"
       "@@if (foo) {\n"
       "@@$$this_is_foo();\n"
       "@@$$that_is_foo();\n"
       "@@}\n"
       "@@else {\n"
       "@@$$testme();\n"
       "@@$$if (bar) {\n"
       "@@$$@@baz();\n"
       "@@$$@@tas();\n"
       "\n"
       "@@$$@@struct Foo {\n"
       "@@$$@@$$$$.value = 10,\n"
       "@@$$@@$$$$otherwise = 15,\n"
       "@@$$@@};\n"
       "@@$$@@struct Bar {\n"
       "#ifdef __linux__\n"
       "@@$$@@$$$$.value = 10,\n"
       "#endif\n"
       "@@$$@@$$$$otherwise = 15,\n"
       "@@$$@@};\n"
       "@@$$}\n"
       "@@}\n"
       "}\n"
       "\n"
       "\n"
       "/* Test foo. */\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest c-comments-and-blank-lines ()
  "Indentation must be detected from code, not comments or blank lines.
Each is the first indented line of its block, otherwise the scan
stops before reaching it."
  (let ((buf (generate-new-buffer "untitled.c")))
    (with-current-buffer buf
      (c-mode)
      (setq-local tab-width 2)

      (insert
       ;; A comment lined up with something other than the code around it.
       "int a(void)\n"
       "{\n"
       "@@    /* Over-indented comment. */\n"
       "@@foo();\n"
       "@@if (x) {\n"
       "@@$$bar();\n"
       "@@}\n"
       "}\n"
       "\n"
       ;; A line of only blank-space.
       "int b(void)\n"
       "{\n"
       "@@    \n"
       "@@foo();\n"
       "@@if (x) {\n"
       "@@$$bar();\n"
       "@@}\n"
       "}\n"
       "\n"
       ;; The line a comment ends on, with code following it.
       "int c(void)\n"
       "{\n"
       "@@/* Comment\n"
       "@@       continued.\n"
       "@@ */ foo();\n"
       "@@if (x) {\n"
       "@@$$bar();\n"
       "@@}\n"
       "}\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest c++-angle-brackets ()
  "Complex C test."
  (let ((buf (generate-new-buffer "untitled.cc")))
    (with-current-buffer buf
      (c++-mode)
      (setq-local tab-width 2)

      (insert
       "static void function()\n"
       "{\n"
       "@@MyStruct *data = static_cast<MyStruct *>(nullptr);\n"
       "@@for (int i = 0; i < 10; i++) {\n"
       "@@$${\n"
       "@@$$@@/* A */\n"
       "@@$$@@{\n"
       "@@$$@@$$/* B */\n"
       "@@$$@@$${\n"
       "@@$$@@$$@@/* C */\n"
       "@@$$@@$$}\n"
       "@@$$@@}\n"
       "@@$$}\n"
       "@@}\n"
       "}\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest cmake-simple ()
  "Simple CMake test."
  (let ((buf (generate-new-buffer "untitled.cmake")))
    (with-current-buffer buf
      ;; CMake is a 3rd party package, fake the mode.
      (setq-local major-mode 'cmake-mode)
      (setq-local tab-width 2)
      (insert
       "if(TRUE)\n"
       "@@message(STATUS \"This is true\")\n"
       "else()\n"
       "@@message(STATUS \"This is false\")\n"
       "@@foreach(X IN MY_LIST)\n"
       "@@$$message(STATUS \"List item X\")\n"
       "@@endforeach()\n"
       "endif()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest cmake-command-name-boundaries ()
  "Commands that merely end with a keyword must not begin or end a block."
  (let ((buf (generate-new-buffer "untitled.cmake")))
    (with-current-buffer buf
      ;; CMake is a 3rd party package, fake the mode.
      (setq-local major-mode 'cmake-mode)
      (setq-local tab-width 2)
      (insert
       "if(TRUE)\n"
       "@@my_endif(a)\n"
       "@@my_if(b)\n"
       "@@foreach(X IN MY_LIST)\n"
       "@@$$my_endforeach(c)\n"
       "@@$$message(d)\n"
       "@@endforeach()\n"
       "endif()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest cmake-stray-end-command ()
  "An end command that opens no block must not abandon the rest of the buffer."
  (let ((buf (generate-new-buffer "untitled.cmake")))
    (with-current-buffer buf
      ;; CMake is a 3rd party package, fake the mode.
      (setq-local major-mode 'cmake-mode)
      (setq-local tab-width 2)
      (insert
       "endif()\n"
       "if(TRUE)\n"
       "@@message(a)\n"
       "@@foreach(X)\n"
       "@@$$message(b)\n"
       "@@endforeach()\n"
       "endif()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest cmake-unterminated-block ()
  "A block with no end command must still begin where it was opened."
  (let ((buf (generate-new-buffer "untitled.cmake")))
    (with-current-buffer buf
      ;; CMake is a 3rd party package, fake the mode.
      (setq-local major-mode 'cmake-mode)
      (setq-local tab-width 2)
      (insert "if(TRUE)\n" "@@foreach(X IN MY_LIST)\n" "@@$$message(a)\n" "@@endforeach()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-simple ()
  "Simple Python test."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "if True:\n"
       "@@@@print('This is true')\n"
       "else:\n"
       "@@@@print('This is false')\n"
       "@@@@for x in range(10):\n"
       "@@@@$$$$print('List item X', x)\n"
       "\n"
       "@@@@x = 10\n"
       "@@@@y = 11\n"
       "@@@@if True:\n"
       "@@@@$$$$print('This is true')\n"
       "@@@@else:\n"
       "@@@@$$$$print('This is false')\n"
       "@@@@$$$$for x in range(10):\n"
       "@@@@$$$$@@@@print('List item X', x)\n"
       "@@@@$$$$@@@@while i < 10:\n"
       "@@@@$$$$@@@@$$$$print(8 + i)\n"
       "@@@@$$$$@@@@$$$$for j in range(10):\n"
       "@@@@$$$$@@@@$$$$@@@@print(j)\n"
       "\n"
       "@@@@$$$$x = 10\n"
       "@@@@$$$$y = 11\n"
       "\n"
       "def foo():\n"
       "@@@@print('Finish')\n"
       "\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-permutations ()
  "Various permutations of indentation and white space Python test."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert "if True:\n" "\n" "@@@@print('This is true')\n")

      (insert "if True:\n" "\n" "# This is a comment." "\n" "@@@@print('This is true')\n")

      (insert
       "if True:\n" "\n" "@@@@# This is an indented comment.\n" "\n" "@@@@print('This is true')\n")

      (insert
       "if True:\n"
       "\n"
       "@@@@    # This is an EXTRA indented comment.\n"
       "\n"
       "@@@@print('This is true')\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-complex ()
  "Complex Python test."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       ;; Ensure a wrapped functions work.
       "def function_01(\n"
       "    a=':',\n"
       "    b = 1: float,\n"
       "    d = {2: 3},\n"
       "    e = ':',\n"
       "):\n"
       "@@@@print('Finish')\n"
       "\n"
       ;; The same again, but not wrapped.
       "def function_2(a=':', b = 1: float, d = {2: 3}, e = ':'):\n"
       "@@@@print('Finish')\n"
       "\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-under-indented-multi-line-string ()
  "A block must not end at a multi-line string that is under-indented."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert "def f():\n" "@@@@print(\"\"\"Summary line.\n")
      ;; The padding is what makes this fail, `syntax-ppss' re-parses from the
      ;; top while the last position it stored is within 2500 characters, so
      ;; the string has to be longer than that before a parse resumes inside it.
      ;; Lines indented to the block are never asked, only the two below are.
      (dotimes (i 80)
        (insert (format "@@@@padding line %d of the summary text.\n" i)))
      (insert "@@under indented one\n" "@@under indented two\n" "\"\"\")\n" "@@@@return 1\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-block-deeper-than-the-range ()
  "A block deeper than the level being scanned must not error.
Narrowing leaves the command that opened the block outside the accessible
buffer, so there is nothing to expand the beginning back to and the first
block found is deeper than the outermost level.
Blocks after it must be neither nested into it nor left out of the tree."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "def outer(a):\n" "    if a:\n" "@@@@@@@@pass\n"
       ;; A block at the same indentation is a sibling, not nested inside.
       "    if b:\n" "@@@@@@@@other()\n" "\n"
       ;; A block below them must still be found,
       ;; it is less indented so it ends the scan of those blocks.
       "def g():\n" "@@@@body()\n")

      ;; Hide the command that opened the block, as narrowing to a region does.
      (narrow-to-region
       (save-excursion
         (goto-char (point-min))
         (forward-line 1)
         (point))
       (point-max))

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-range-starting-in-a-multi-line-string ()
  "A range starting on an un-indented line of a multi-line string is mid-block.
The enclosing commands are before it, so the beginning must expand back
to them, exactly as it does for a line inside brackets."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "def f(a):\n"
       ;; Note that this line is before the range so it isn't highlighted,
       ;; expanding the beginning back to it only builds the tree from it.
       "    s = \"\"\"\n" "text at column zero\n" "\"\"\"\n" "@@@@if a:\n" "@@@@$$$$pass\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@ 3)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-async-blocks ()
  "Every `async' block command opens a block, not only `async def'.
The plain commands are included for contrast, they are unaffected."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "async def run(cm, items):\n"
       "@@@@async with cm as c:\n"
       "@@@@$$$$await c.go()\n"
       "@@@@async for i in items:\n"
       "@@@@$$$$await i.go()\n"
       "@@@@with cm as c:\n"
       "@@@@$$$$c.go()\n"
       "@@@@for i in items:\n"
       "@@@@$$$$i.go()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-block-ending-in-a-multi-line-string ()
  "A multi-line string extends the block it is part of.
Its lines may be indented less than the block, but unlike blank lines
they are inside it, so the block does not end before them."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert "def f():\n" "@@@@x = 1\n" "@@@@s = \"\"\"doc\n" "@@@under indented\n" "@@@\"\"\"\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-range-starting-at-a-column-zero-comment ()
  "A comment at column zero does not mean the range is at the outermost level.
Comments sit at any indentation, commented out code and section markers
are routinely left at column zero within a block, so the beginning must
still expand back to the commands that opened it."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       ;; Note that these lines are before the range so they aren't
       ;; highlighted, expanding back to them only builds the tree.
       "class C:\n"
       "    def a(self):\n"
       "        pass\n"
       "\n"
       "## comment at column zero\n"
       "\n"
       "@@@@def b(self):\n"
       "@@@@$$$$pass\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@ 5)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-range-of-indented-comments ()
  "A range holding nothing but indented comments is still mid-block.
Being indented they answer this on their own, so unlike a comment at
column zero they must not be skipped over."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "class C:\n"
       "    def a(self):\n"
       "@@@@$$$$# comment one\n"
       "@@@@$$$$# comment two\n"
       "@@@@$$$$# comment three\n"
       ;; Note that the range ends above this line, so it isn't highlighted.
       ;; The comments are all there is to answer from.
       "        pass\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@ 3 5)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-block-end-past-a-dedent ()
  "A block must not extend past the line that dedents out of it.
The overshoot is only visible once it lands on the continuation line of
the statement below, which is indented enough to be taken for the body."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "def f(dirlist):\n"
       "@@@@for d in dirlist:\n"
       "@@@@$$$$for seq in range(10):\n"
       "@@@@$$$$@@@@try:\n"
       "@@@@$$$$@@@@$$$$pass\n"
       "@@@@$$$$@@@@except OSError:\n"
       "@@@@$$$$@@@@$$$$break\n"
       "@@@@raise Error(a,\n"
       "@@@@            b)\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-block-ending-on-a-blank-indented-line ()
  "A block must not end on a blank line, even one indented to it.
Such a line counts as part of the block while scanning, so stepping back
off the line that dedented out of it can land on one."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "def f():\n" "@@@@for d in x:\n" "@@@@$$$$pass\n"
       ;; Blank, but indented as far as the block body.
       "        \n" "top_level()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest python-block-with-an-under-indented-bracket ()
  "Lines continuing a bracket are within the block whatever their indentation.
A closing bracket is often placed below the indentation of the block it
is in, which must not end the block there."
  (let ((buf (generate-new-buffer "untitled.py")))
    (with-current-buffer buf
      (setq python-indent-guess-indent-offset nil)
      (python-mode)
      (setq tab-width 4)

      (insert
       "def f():\n" "@@@@d = {\n" "@@@@    'a': 1,\n"
       ;; Indented less than the block body.
       "@@@}\n" "@@@@return d\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest mode-disable-while-narrowed ()
  "Disabling the mode must remove every overlay, narrowing or not.
Anything left outside the narrowing stays for good, the mode is no
longer enabled to clean up after itself."
  (let ((buf (generate-new-buffer "untitled.c")))
    (with-current-buffer buf
      (c-mode)
      (setq-local tab-width 2)

      (insert
       "int a(void)\n"
       "{\n"
       "  if (x) {\n"
       "    foo();\n"
       "  }\n"
       "}\n"
       "\n"
       "int b(void)\n"
       "{\n"
       "  if (y) {\n"
       "    bar();\n"
       "  }\n"
       "}\n"
       "\n"
       "int c(void)\n"
       "{\n"
       "  if (z) {\n"
       "    baz();\n"
       "  }\n"
       "}\n")

      (hl-indent-scope-mode)
      (hl-indent-scope-buffer)
      (should (hl-indent-scope-test--from-overlays))

      ;; Narrow to the middle function, as `narrow-to-region' does,
      ;; leaving overlays on both sides of it.
      (narrow-to-region
       (hl-indent-scope-test--pos-from-line 8) (hl-indent-scope-test--pos-from-line 14))
      (hl-indent-scope-mode -1)
      (widen)

      (should (null (hl-indent-scope-test--from-overlays))))))

(provide 'hl-indent-scope-test)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-test.el ends here
