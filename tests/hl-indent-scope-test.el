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

(defun hl-indent-scope-test--tree-from-code (mode code)
  "Return the indentation scope tree for CODE using the preset for MODE."
  (with-temp-buffer
    (funcall mode)
    (insert code)
    (hl-indent-scope-preset)
    ;; Note that presets which replace the tree extraction entirely
    ;; must be called through their own callback, as font-locking does.
    (save-excursion
      (funcall
       (or hl-indent-scope-tree-fn 'hl-indent-scope--tree-from-buffer) (point-min) (point-max)))))

(defun hl-indent-scope-test--tree-depth (tree)
  "Return the number of levels TREE nests, zero when it is empty."
  (let ((depth 0))
    (dolist (branch tree)
      (setq depth (max depth (1+ (hl-indent-scope-test--tree-depth (cdr branch))))))
    depth))

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

(defun hl-indent-scope-test--extern-block-layouts (mode)
  "Check that extern block layouts are hidden by the preset for MODE."
  (dolist (code-extern
           '("extern \"C\"\n{\n"
             "extern \"C\" /* comment */ {\n"
             "extern /* comment */ \"C\" {\n"
             "extern \"C++\" {\n"))
    ;; Only the function may add a level, checking the depth instead of an
    ;; empty tree ensures code that fails to parse can't pass the test.
    (should
     (equal
      1
      (hl-indent-scope-test--tree-depth
       (hl-indent-scope-test--tree-from-code
        mode (concat code-extern "int a(void)\n{\nfoo();\n}\n}\n")))))))

(ert-deftest c-extern-block-layouts ()
  "Extern blocks remain hidden when whitespace, comments, or linkage vary."
  (hl-indent-scope-test--extern-block-layouts #'c-mode))

(ert-deftest c++-extern-block-layouts ()
  "Extern blocks remain hidden when whitespace, comments, or linkage vary."
  (hl-indent-scope-test--extern-block-layouts #'c++-mode))

(defun hl-indent-scope-test--extern-keyword-boundary (mode)
  "Check that an identifier ending in `extern' is not extern in MODE."
  ;; Both the outer block and the function add a level.
  (should
   (equal
    2
    (hl-indent-scope-test--tree-depth
     (hl-indent-scope-test--tree-from-code
      mode "notextern \"C\" {\nint a(void)\n{\nfoo();\n}\n}\n")))))

(ert-deftest c-extern-keyword-boundary ()
  "An `extern' suffix in an identifier must not hide a C block."
  (hl-indent-scope-test--extern-keyword-boundary #'c-mode))

(ert-deftest c++-extern-keyword-boundary ()
  "An `extern' suffix in an identifier must not hide a C++ block."
  (hl-indent-scope-test--extern-keyword-boundary #'c++-mode))

(ert-deftest c++-namespace-block-layouts ()
  "Namespace blocks remain hidden when whitespace, comments or nesting vary."
  (dolist (code-namespace
           '("namespace foo {\n"
             "namespace foo\n{\n"
             "namespace foo /* comment */ {\n"
             "namespace /* comment */ foo {\n"
             "namespace {\n"
             "namespace foo::bar {\n"
             "inline namespace foo {\n"
             "namespace foo::inline bar {\n"))
    ;; Only the function may add a level, checking the depth instead of an
    ;; empty tree ensures code that fails to parse can't pass the test.
    (should
     (equal
      1
      (hl-indent-scope-test--tree-depth
       (hl-indent-scope-test--tree-from-code
        #'c++-mode (concat code-namespace "int a(void)\n{\nfoo();\n}\n}\n")))))))

(ert-deftest c++-namespace-keyword-boundary ()
  "A `namespace' within an identifier must not hide a C++ block."
  (dolist (code-block
           '("struct mynamespace {\n"
             "struct not_a_namespace_type {\n"
             "class Foo : public Bar {\n"
             "struct A::B {\n"))
    ;; Both the outer block and the function add a level.
    (should
     (equal
      2
      (hl-indent-scope-test--tree-depth
       (hl-indent-scope-test--tree-from-code
        #'c++-mode (concat code-block "int a(void)\n{\nfoo();\n}\n}\n")))))))

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

(ert-deftest cmake-block ()
  "The `block' command, added in CMake 3.25, is a block."
  (let ((buf (generate-new-buffer "untitled.cmake")))
    (with-current-buffer buf
      ;; CMake is a 3rd party package, fake the mode.
      (setq-local major-mode 'cmake-mode)
      (setq-local tab-width 2)
      (insert
       "block(SCOPE_FOR VARIABLES)\n"
       "@@set(x 1)\n"
       "@@if(TRUE)\n"
       "@@$$message(a)\n"
       "@@endif()\n"
       "endblock()\n"
       "message(b)\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(defun hl-indent-scope-test--cmake-mode ()
  "Fake `cmake-mode', which is a 3rd party package."
  (setq-local major-mode 'cmake-mode))

(ert-deftest cmake-command-in-arguments ()
  "A command only begins a line, so one within arguments is an argument.
Otherwise the `if' here opens a block that's never closed,
adopting the loop after it."
  (should
   (equal
    1
    (hl-indent-scope-test--tree-depth
     (hl-indent-scope-test--tree-from-code
      #'hl-indent-scope-test--cmake-mode
      "set(x\n  if(y)\n)\nforeach(i a b)\n  message(i)\nendforeach()\n")))))

(ert-deftest cmake-unclosed-arguments ()
  "An argument list that isn't closed, as while typing, doesn't hide what follows.
So both blocks are found."
  (should
   (equal
    2
    (length
     (hl-indent-scope-test--tree-from-code
      #'hl-indent-scope-test--cmake-mode
      "foreach(x\n  message(a)\nendforeach()\nwhile(TRUE)\n  message(b)\nendwhile()\n")))))

(ert-deftest cmake-close-ends-a-block-left-open ()
  "An end command closes a block left open within its block.
As it would be while being typed, otherwise the end command closes the
block left open instead, leaving its own block open to the end of the
buffer, adopting the function after it."
  (should
   (equal
    '(((52 . 61)) ((10 . 29) ((18 . 29))))
    (hl-indent-scope-test--tree-from-code
     #'hl-indent-scope-test--cmake-mode
     "function(f)\n  if(a)\n    x()\nendfunction()\nfunction(g)\n  y()\nendfunction()\n")))
  ;; The reverse, a function left open within an `if'.
  (should
   (equal
    '(((46 . 55)) ((4 . 29) ((18 . 29))))
    (hl-indent-scope-test--tree-from-code
     #'hl-indent-scope-test--cmake-mode
     "if(a)\n  function(f)\n    x()\nendif()\nfunction(g)\n  y()\nendfunction()\n"))))

(ert-deftest cmake-bracket-syntax ()
  "Bracket comments & arguments are skipped, along with what they contain.
This needs `cmake-mode', which gives them their syntax."
  (skip-unless (require 'cmake-mode nil t))
  (should
   (equal
    1
    (length
     (hl-indent-scope-test--tree-from-code
      #'cmake-mode
      (concat
       "#[[\n"
       "if(TRUE)\n"
       "]]\n"
       "message([[ ) endif() ]])\n"
       "foreach(i a b)\n"
       "  message(i)\n"
       "endforeach()\n"))))))

(ert-deftest cmake-case-insensitive ()
  "CMake commands are case-insensitive."
  (let ((buf (generate-new-buffer "untitled.cmake")))
    (with-current-buffer buf
      ;; CMake is a 3rd party package, fake the mode.
      (setq-local major-mode 'cmake-mode)
      (setq-local tab-width 2)
      (insert
       "IF(TRUE)\n"
       "@@Foreach(X IN MY_LIST)\n"
       "@@$$message(a)\n"
       "@@EndForEach()\n"
       "ENDIF()\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(defun hl-indent-scope-test--fish-mode ()
  "Fake `fish-mode', which is a 3rd party package.
Unlike CMake the mode symbol isn't enough, the preset reads the
syntax-table to skip strings & comments, so the table is needed too.
This matches the one `fish-mode' defines, which inherits from
`text-mode', giving `|', `&', `<' & `>' symbol syntax."
  (setq-local major-mode 'fish-mode)
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?' "\"'" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$ "'" table)
    (set-syntax-table table))
  ;; `fish_indent' indents 4 wide.
  (setq-local tab-width 4))

(ert-deftest fish-simple ()
  "Simple fish test."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "function foo\n"
       "@@@@if test -f file\n"
       "@@@@$$$$echo yes\n"
       "@@@@else\n"
       "@@@@$$$$echo no\n"
       "@@@@$$$$for i in a b c\n"
       "@@@@$$$$@@@@echo item\n"
       "@@@@$$$$end\n"
       "@@@@end\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-command-position ()
  "A keyword only ends a block in command position.
Everywhere else it's an argument, a name or text."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "while true\n"
       "@@@@echo end\n"
       "@@@@echo if\n"
       "@@@@my-end\n"
       "@@@@# end\n"
       "@@@@echo \"end\"\n"
       "@@@@echo 'end'\n"
       "@@@@set x end\n"
       ;; These run a program called `end'.
       "@@@@command end\n"
       "@@@@builtin end\n"
       "@@@@exec end\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-command-position-separators ()
  "Every separator that begins a command must be recognized.
A block opened after one of these is a block like any other,
so failing to see it drops the level from there on."
  (dolist (code-open
           '("begin\n"
             "true; begin\n"
             "true | begin\n"
             "true && begin\n"
             "true || begin\n"
             "and begin\n"
             "not begin\n"
             "! begin\n"
             ;; Continued lines.
             "true | \\\nbegin\n"
             "and \\\nbegin\n"
             "and not begin\n"
             "time begin\n"))
    ;; Both the outer block and the inner one add a level.
    (should
     (equal
      2
      (hl-indent-scope-test--tree-depth
       (hl-indent-scope-test--tree-from-code
        #'hl-indent-scope-test--fish-mode
        (concat "begin\n" code-open "echo a\n" "end\n" "end\n")))))))

(ert-deftest fish-keyword-next-to-an-operator ()
  "A keyword may be followed by an operator without a space.
Since `fish-mode' gives these symbol syntax, the keyword is then part
of a longer symbol.  Each block must be found, so the block after it
is a sibling, not a child."
  (dolist (code
           '("for x in a\necho a\nend|sort\n"
             "for x in a\necho a\nend>/dev/null\n"
             "for x in a\necho a\nend&\n"
             "cat x|while read l\necho a\nend\n"))
    (should
     (equal
      2
      (length
       (hl-indent-scope-test--tree-from-code
        #'hl-indent-scope-test--fish-mode (concat code "begin\necho b\nend\n")))))))

(ert-deftest fish-keyword-at-the-end-of-a-line ()
  "A keyword may be followed by a line continuation or a carriage return.
The carriage return is left by mixed line endings.  Each block must be
found, so the block after it is a sibling, not a child."
  (dolist (code '("for x in a\necho a\nend\\\n| sort\n" "begin\r\necho a\r\nend\r\n"))
    (should
     (equal
      2
      (length
       (hl-indent-scope-test--tree-from-code
        #'hl-indent-scope-test--fish-mode (concat code "begin\necho b\nend\n")))))))

(ert-deftest fish-keyword-within-a-word ()
  "A keyword followed by any other character is part of a longer word.
So `end_x' is a command that doesn't close a block."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "begin\n"
       "@@@@end_x\n"
       "@@@@end-x\n"
       "@@@@end=x\n"
       "@@@@echo a\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-command-position-within-a-token ()
  "A command may begin within a token, after a separator or a brace.
Without a space the separator & the decorator after it are one token,
so the decorator has to be found within it."
  (dolist (code-and-depth
           '(("true;and begin\necho a\nend\n" . 1)
             ("echo (not begin\necho a\nend)\n" . 1)
             ;; The brace & the condition are both blocks.
             ("{ if true\necho a\nend }\n" . 2)
             ;; A brace expansion is not.
             ("echo {if,else}\n" . 0)))
    (should
     (equal
      (cdr code-and-depth)
      (hl-indent-scope-test--tree-depth
       (hl-indent-scope-test--tree-from-code
        #'hl-indent-scope-test--fish-mode (car code-and-depth)))))))

(ert-deftest fish-condition-is-a-command ()
  "The condition of `if' & `while' is a command, so it may open a block.
Finding it is what matters, otherwise its `end' closes the loop &
everything below the condition loses a level.
Note that both blocks open on the same line, so no line is indented
to the loop alone & the two levels are detected as one width."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "while begin\n"
       "@@@@@@@@echo condition\n"
       "@@@@end\n"
       "@@@@echo body\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-else-if ()
  "An `else if' continues the block, it doesn't open one.
A block after it does open one though, so its end must not be
taken for the end of the block that `else' continues.
The same goes for any other block after `else'."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "if false\n"
       "@@@@echo a\n"
       "else if true\n"
       "@@@@echo b\n"
       "else if begin ; true ; end\n"
       "@@@@echo c\n"
       "else\n"
       "@@@@echo d\n"
       "end\n"
       ;; Any other block after `else' is one.
       "if false\n"
       "@@@@echo a\n"
       "else begin\n"
       "@@@@$$$$echo b\n"
       "@@@@end\n"
       "@@@@echo c\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-variable-assignment-prefix ()
  "A command may be prefixed by variable assignments, `a=b command'."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "a=b switch x\n"
       "@@@@case x\n"
       "@@@@$$$$c=d begin\n"
       "@@@@$$$$@@@@echo a\n"
       "@@@@$$$$end\n"
       ;; A name may begin with a digit.
       "@@@@$$$$1=d begin\n"
       "@@@@$$$$@@@@echo b\n"
       "@@@@$$$$end\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-brace-block ()
  "A brace compound statement is a block, brace expansion is not."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "{\n"
       "@@@@echo a\n"
       ;; An escaped brace is text.
       "@@@@echo \\}\n"
       "@@@@touch file/{one,two}.txt\n"
       ;; An expansion opening its own token, the shape a block also has.
       "@@@@echo {one,two}\n"
       "@@@@if true\n"
       "@@@@$$$$echo b\n"
       "@@@@end\n"
       "}\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-brace-block-closes-a-block-left-open ()
  "A brace closes a keyword block left open within the braces.
As it would be while being typed, otherwise the braces stay open to
the end of the buffer, adopting the block after them."
  (should
   (equal
    '(((25 . 31)) ((2 . 18) ((5 . 18))))
    (hl-indent-scope-test--tree-from-code
     #'hl-indent-scope-test--fish-mode "{\nif true\necho a\n}\nbegin\necho\nend\n"))))

(ert-deftest fish-brace-block-as-one-token ()
  "A brace block written as a single token is still a block.
Its opening brace is within the token as brace expansion is, so the
closing brace has to ask whether that brace began a command.  Without
it the block never closes & adopts what follows as its own contents."
  ;; The block & the loop are siblings, both at the outermost level.
  (should
   (equal
    1
    (hl-indent-scope-test--tree-depth
     (hl-indent-scope-test--tree-from-code
      #'hl-indent-scope-test--fish-mode "{echo}\nif true\necho x\nend\n")))))

(ert-deftest fish-brace-block-closes-after-an-expansion ()
  "A brace expansion ending a token doesn't take the brace after it.
Otherwise the block never closes & adopts what follows as its own
contents, so the two blocks are siblings."
  (should
   (equal
    2
    (length
     (hl-indent-scope-test--tree-from-code
      #'hl-indent-scope-test--fish-mode "{\necho a{b,c}}\nif true\necho x\nend\n")))))

(ert-deftest fish-brace-block-closes-a-command ()
  "The brace that closes a block may follow a command instead of beginning one.
Only the block it closes matters here, the block is on one line so it
has no indentation of its own to show."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "for i in a b\n"
       "@@@@true || { set ok false; break }\n"
       "@@@@echo b\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-brace-expansion-over-lines ()
  "A brace expansion spanning lines must not end the block it's within.
Its closing brace begins a line & so looks like the end of a compound
statement.  A loop is opened by a keyword, not a brace, while a brace
block has to pair the brace with the one that opened it."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "for foo in {a,\n"
       "b}\n"
       "@@@@echo foo\n"
       "end\n"
       ;; Within a brace block the kinds match, so the block has to tell.
       "{\n"
       "@@@@echo {a,\n"
       "b}\n"
       "@@@@echo foo\n"
       "}\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-line-continuation ()
  "A keyword on a continued line is an argument, not a command."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "if true\n"
       ;; Note the continued line is indented past the block,
       ;; only the block's own level is filled.
       "@@@@echo a \\\n"
       "@@@@    end\n"
       "@@@@echo b\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-comment-ending-in-a-backslash ()
  "A backslash within a comment is comment text, it doesn't continue the line.
Taking it for a continuation rejects the command below it as an
argument, so the block stays open to the end of the buffer."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert "if true\n" "@@@@echo a # c\\\n" "end\n" "echo after\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-comment-within-a-word ()
  "A `#' within a word is text, as fish only begins a comment at a token.
So the `end' after it closes the block, while `fish-mode' gives every
`#' comment syntax.  Taking it for a comment leaves the block open,
nesting the block after it."
  (dolist (code-and-tree
           '(("if true\necho a#b; end\nbegin\necho\nend\n" . (((28 . 34)) ((3 . 19))))
             ("if true\necho a#b#c; end\nbegin\necho\nend\n" . (((30 . 36)) ((3 . 21))))
             ;; The brace of the expansion between them pairs, closing at its brace.
             ("{\necho a#{b,#c}; }\nbegin\necho\nend\n" . (((25 . 31)) ((2 . 18))))
             ;; The backslash continues the line, so the `end' is an argument.
             ("if true\necho a#b \\\nend\nend\n" . (((3 . 24))))
             ;; A `#' beginning a token is a comment.
             ("if true\necho a;# end\nbegin\necho\nend\n" . (((3 . 33) ((27 . 33)))))
             ("if true\necho a#b #; end\nbegin\necho\nend\n" . (((3 . 36) ((30 . 36)))))
             ("{\necho a;#}\necho b\n}\n" . (((2 . 20))))))
    (should
     (equal
      (cdr code-and-tree)
      (hl-indent-scope-test--tree-from-code
       #'hl-indent-scope-test--fish-mode (car code-and-tree))))))

(ert-deftest fish-switch-case ()
  "Each `case' clause is a level of its own.
That is how `fish_indent' lays them out, so the body of a clause is
indented once past the `switch' and once past the `case'."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       ;; Note that `$' is used as a marker character by this test,
       ;; so variable expansion can't be used in the code.
       "switch (uname)\n"
       "@@@@case a\n"
       "@@@@$$$$echo a\n"
       "@@@@case b c\n"
       "@@@@$$$$echo b\n"
       "@@@@$$$$if true\n"
       "@@@@$$$$@@@@echo c\n"
       "@@@@$$$$end\n"
       "@@@@case '*'\n"
       "@@@@$$$$echo other\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-stray-end-command ()
  "An end command that opens no block must not abandon the rest of the buffer."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "end\n"
       "if true\n"
       "@@@@echo a\n"
       "@@@@for i in x\n"
       "@@@@$$$$echo b\n"
       "@@@@end\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-unterminated-block ()
  "A block with no end command must still begin where it was opened."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert "if true\n" "@@@@for i in x\n" "@@@@$$$$echo a\n" "@@@@end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-unterminated-block-in-range ()
  "An un-terminated block must not depend on its nested blocks being in range.
Its end is taken from the last nested block, which is past the range."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "function foo\n"
       "@@@@echo a\n"
       "@@@@echo b\n"
       ;; Note that the range ends above this line, so it isn't highlighted.
       "    if true\n"
       "        echo c\n"
       "    end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@ 1 3)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest fish-single-line-block ()
  "A block that opens & closes on one line has no indentation to show."
  (let ((buf (generate-new-buffer "untitled.fish")))
    (with-current-buffer buf
      (hl-indent-scope-test--fish-mode)
      (insert
       "begin; echo a; end\n"
       "if true\n"
       "@@@@begin; echo b; end\n"
       "@@@@echo c\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

;; The tests below call the preset's functions directly.
(require 'hl-indent-scope-preset-fish-mode)

(ert-deftest fish-command-search-stops-at-the-limit ()
  "Rejected matches after the range aren't examined.
Otherwise every request for a range examines up to the next command found,
which in a file of few blocks is the rest of the buffer."
  (let ((positions nil)
        (keyword-p (symbol-function 'hl-indent-scope-preset-fish--keyword-p)))
    (cl-letf (((symbol-function 'hl-indent-scope-preset-fish--keyword-p)
               (lambda (pos)
                 (push pos positions)
                 (funcall keyword-p pos))))
      ;; The range ends after the first block, the keywords after it are arguments.
      (should
       (equal
        '(((6 . 9)))
        (hl-indent-scope-test--keyword-tree
         "begin\na\nend\necho begin\necho begin\n" #'hl-indent-scope-preset-fish--next-fn
         14))))
    (should positions)
    (dolist (pos positions)
      (should (< pos 14)))))

(ert-deftest lua-simple ()
  "Simple Lua test."
  (let ((buf (generate-new-buffer "untitled.lua")))
    (with-current-buffer buf
      (lua-mode)
      (insert
       "local function foo(a, b)\n"
       "@@if a then\n"
       "@@$$for i = 1, 10 do\n"
       "@@$$@@print(i)\n"
       "@@$$end\n"
       "@@elseif b then\n"
       "@@$$print(b)\n"
       "@@else\n"
       "@@$$repeat\n"
       "@@$$@@a = a - 1\n"
       "@@$$until a == 0\n"
       "@@end\n"
       "@@while a do\n"
       "@@$$a = nil\n"
       "@@end\n"
       "end\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest lua-condition-over-lines ()
  "The body of an `if' begins after its `then'."
  (should
   (equal
    '(((20 . 27)))
    (hl-indent-scope-test--tree-from-code #'lua-mode "if a and\n    b then\n  x()\nend\n"))))

(ert-deftest lua-parameters-over-lines ()
  "The body of a `function' begins after its parameters, comments included."
  (should
   (equal
    '(((37 . 44)))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "function f(a, -- first\n           b)\n  x()\nend\n"))))

(ert-deftest lua-function-in-condition ()
  "A function in a condition is skipped along with it, keeping the `then'.
Both in brackets & at the depth of the `if', where its blocks are counted."
  (should
   (equal
    '(((40 . 47)))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "if f(function() if a then end end) then\n  x()\nend\n")))
  (should
   (equal
    '(((48 . 55)))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "if f == function()\n    if a then end\n  end then\n  x()\nend\n")))
  (should
   (equal
    '(((43 . 50)))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "if f == function() repeat until a end then\n  x()\nend\n"))))

(ert-deftest lua-missing-then ()
  "An `if' without a `then' doesn't take the `then' of the block after it."
  (should
   (equal
    '(((3 . 32) ((17 . 28))))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "if a\n  if b then\n    x()\n  end\nend\n")))
  ;; A function after it is skipped, its `end' doesn't end the search either.
  (should
   (equal
    '(((3 . 57) ((42 . 53)) ((26 . 27))))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "if a\n  local function g() end\n  if b then\n    x()\n  end\nend\n"))))

(ert-deftest lua-missing-parenthesis ()
  "A `function' missing a parenthesis doesn't take the call after it as parameters."
  (should
   (equal
    '(((9 . 20)))
    (hl-indent-scope-test--tree-from-code #'lua-mode "function f(a\n  g()\nend\n")))
  (should
   (equal
    '(((9 . 18)))
    (hl-indent-scope-test--tree-from-code #'lua-mode "function f\n  g()\nend\n"))))

(ert-deftest lua-repeat-is-its-own-kind ()
  "An `end' never closes a `repeat' & an `until' never closes another block."
  (should
   (equal
    '(((37 . 54)) ((7 . 20)))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "repeat\n  end\n  x()\nuntil a\nif b then\n  until c\n  y()\nend\n"))))

(ert-deftest lua-until-closes-a-block-left-open ()
  "An `until' closes a block left open within its `repeat'.
An `end' does the same for a `repeat' left open within an `if'.  Either is
left open while being typed, otherwise the outer block stays open to the
end of the buffer, adopting the block after it."
  (should
   (equal
    '(((45 . 52)) ((7 . 28) ((19 . 28))))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "repeat\n  if a then\n    y()\nuntil b\nif c then\n  z()\nend\n")))
  (should
   (equal
    '(((41 . 48)) ((10 . 28) ((19 . 28))))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode "if c then\n  repeat\n    y()\nend\nif c then\n  z()\nend\n"))))

(ert-deftest lua-keywords-in-strings-and-comments ()
  "Keywords in strings & comments, including multi-line ones, aren't blocks."
  (should
   (equal
    '(((10 . 78)))
    (hl-indent-scope-test--tree-from-code
     #'lua-mode
     (concat
      "if a then\n"
      "  s = \"end\" -- end\n"
      "  --[[\n"
      "  end\n"
      "  ]]\n"
      "  t = [==[\n"
      "  end\n"
      "  ]==]\n"
      "  x()\n"
      "end\n")))))

(ert-deftest makefile-simple ()
  "Simple Makefile test, note that `$' can't be used in the code."
  (let ((buf (generate-new-buffer "Makefile")))
    (with-current-buffer buf
      (makefile-gmake-mode)
      (insert
       "ifdef A\n"
       "@@X = 1\n"
       "@@ifeq (a,b)\n"
       "@@$$Y = 2\n"
       "@@else ifeq (a,c)\n"
       "@@$$Y = 3\n"
       "@@else\n"
       "@@$$Y = 4\n"
       "@@endif\n"
       "endif\n"
       "define T\n"
       "@@Z = 5\n"
       "endef\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest makefile-define-modifiers ()
  "A variable definition may be preceded by modifiers."
  (let ((buf (generate-new-buffer "Makefile")))
    (with-current-buffer buf
      (makefile-gmake-mode)
      (insert
       "override define A\n"
       "@@X\n"
       "endef\n"
       "export define B =\n"
       "@@Y\n"
       "endef\n"
       "ifdef C\n"
       ;; Only a definition may be modified, this exports a variable.
       "@@export endif\n"
       "@@Z\n"
       "endif\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest makefile-directive-names-in-other-uses ()
  "A directive's name followed by an assignment or a colon isn't a directive.
Make reads them as a variable & a target, so the block continues past them."
  (let ((buf (generate-new-buffer "Makefile")))
    (with-current-buffer buf
      (makefile-gmake-mode)
      (insert
       "ifdef A\n"
       "@@X = 1\n"
       "endif = 1\n"
       "endif := 2\n"
       "endif:\n"
       "@@Y = 2\n"
       "endif\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest makefile-definition-is-its-own-kind ()
  "Make doesn't read conditionals in a definition, so an `endif' can't end one."
  (let ((buf (generate-new-buffer "Makefile")))
    (with-current-buffer buf
      (makefile-gmake-mode)
      (insert
       "define T\n"
       "@@endif\n"
       "@@X\n"
       "endef\n")

      (let ((code-str-expect (buffer-substring-no-properties (point-min) (point-max)))
            (code-str-result (hl-indent-scope-test--do-test-on-current-buffer ?$ ?@)))
        (should (equal code-str-expect code-str-result))))))

(ert-deftest makefile-definition-is-isolated ()
  "A stray `endif' in a definition doesn't end a conditional around it.
The `endef' still ends a conditional left open in the definition."
  (should
   (equal
    '(((8 . 34) ((17 . 26))))
    (hl-indent-scope-test--tree-from-code
     #'makefile-gmake-mode "ifdef A\ndefine T\nendif\nX\nendef\nY\nendif\n")))
  (should
   (equal
    '(((36 . 39)) ((9 . 23) ((20 . 23))))
    (hl-indent-scope-test--tree-from-code
     #'makefile-gmake-mode "define X\nifeq (a,b)\nY\nendef\nifdef C\nZ\nendif\n"))))

(ert-deftest makefile-definition-body-directives ()
  "Make reads `define' & `endef' differently in the body of a definition.
A line beginning with a tab is text & only a bare `define' nests,
otherwise the definition ends early or not at all."
  (dolist (code-and-tree
           '(("define X\n\tendef\nY\nendef\n" . (((9 . 19))))
             ("define X\nendef#c\nY\nendef\n" . (((9 . 20))))
             ("define X\noverride define Y\nZ\nendef\nW = 1\n" . (((9 . 30))))
             ;; After a nested definition, which is read as one.
             ("define X\ndefine Y\nendef\noverride define Z\nendef\n" . (((9 . 43) ((18 . 19)))))
             ;; Unlike outside of a definition, it isn't an assignment.
             ("define X\ndefine = 1\nZ\nendef\nendef\n" . (((9 . 29) ((20 . 23)))))))
    (should
     (equal
      (cdr code-and-tree)
      (hl-indent-scope-test--tree-from-code #'makefile-gmake-mode (car code-and-tree))))))

(ert-deftest makefile-continued-lines ()
  "A line ending in a backslash joins the next, which can't begin a directive.
This applies to comments too, unlike most languages."
  (should
   (equal
    '(((24 . 55)))
    (hl-indent-scope-test--tree-from-code
     #'makefile-gmake-mode
     (concat
      "X = a \\\n"
      "ifdef B\n"
      "ifdef A\n"
      "  # comment \\\n"
      "  endif\n"
      "  Y = 1\n"
      "endif\n")))))

(ert-deftest makefile-continued-line-carriage-return ()
  "A carriage return after a backslash doesn't stop it joining the next line.
Make removes the carriage return first, it's left by mixed line endings."
  (should
   (equal
    '(((25 . 32)))
    (hl-indent-scope-test--tree-from-code
     #'makefile-gmake-mode "X = a \\\r\nifdef B\nifdef A\nY = 1\nendif\nendif\n"))))

(ert-deftest makefile-continued-directive ()
  "A directive's body begins after the lines joined to it."
  (should
   (equal
    '(((15 . 24)))
    (hl-indent-scope-test--tree-from-code
     #'makefile-gmake-mode
     (concat "ifeq (a,\\\n" "  b)\n" "  X = 1\n" "endif\n")))))

(ert-deftest makefile-tab-indented-directives ()
  "Directives may be indented by tabs, as make reads them outside of recipes."
  (should
   (equal
    2
    (hl-indent-scope-test--tree-depth
     (hl-indent-scope-test--tree-from-code
      #'makefile-gmake-mode "ifdef A\n\tifeq (a,b)\n\t\tX = 1\n\tendif\nendif\n")))))

(ert-deftest makefile-carriage-return ()
  "A carriage return left by mixed line endings ends a directive's line.
Otherwise the `endif' is missed, nesting the next block in the first."
  (should
   (equal
    2
    (length
     (hl-indent-scope-test--tree-from-code
      #'makefile-gmake-mode "ifdef A\nendif\r\nifdef B\n  X = 1\nendif\n")))))

(ert-deftest makefile-quote-is-not-a-string ()
  "Make has no strings, so a quote doesn't hide the directives after it."
  (should
   (equal
    1
    (length
     (hl-indent-scope-test--tree-from-code
      #'makefile-gmake-mode "$(info don't)\nifdef A\n  X = 1\nendif\n")))))

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

(ert-deftest python-comment-ending-the-buffer ()
  "A comment on the last line, without a line ending, must not hang.
This is typical while typing, where the comment is the first line of a block,
the result must match the same buffer with a line ending."
  (should
   (equal
    '(((11 . 11)))
    (hl-indent-scope-test--tree-from-code #'python-mode "def foo():\n    # comment")))
  (should
   (equal
    '(((11 . 11))) (hl-indent-scope-test--tree-from-code #'python-mode "def foo():\n# comment")))
  (should
   (equal
    '(((21 . 21)) ((6 . 15)))
    (hl-indent-scope-test--tree-from-code #'python-mode "if x:\n    pass\nelse:\n    # todo"))))

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

(ert-deftest mode-disable-after-changing-the-idle-delay ()
  "Disabling must undo the style that was enabled, not the current setting.
`hl-indent-scope-idle-delay' selects between the immediate & timer
styles, and may be changed while the mode is enabled, where the value no
longer matches what is in use."
  (dolist (delay-pair (list (cons 0.2175 0.0) (cons 0.0 0.2175)))
    (let ((buf (generate-new-buffer "untitled.c")))
      (with-current-buffer buf
        (c-mode)
        (insert "int a(void)\n" "{\n" "  foo();\n" "}\n")

        (let ((hl-indent-scope-idle-delay (car delay-pair)))
          (hl-indent-scope-mode))
        ;; The delay is customized while the mode is enabled.
        (let ((hl-indent-scope-idle-delay (cdr delay-pair)))
          (hl-indent-scope-mode -1))

        (let ((fns (bound-and-true-p jit-lock-functions)))
          (should (null (memq 'hl-indent-scope--idle-font-lock-region-pending fns)))
          (should (null (memq 'hl-indent-scope--font-lock-fontify-region fns))))))))

;; ---------------------------------------------------------------------------
;; Keyword Library Tests
;;
;; These drive `hl-indent-scope-lib-keyword-tree-fn' directly, with a scanner
;; written for the test, so the engine can be tested apart from any preset.

(require 'hl-indent-scope-lib-keyword)

(defun hl-indent-scope-test--keyword-next-fn (table)
  "Return a function finding the words in TABLE, a list of (WORD TYPE KIND).
Words are lower case letters, or a single brace."
  (lambda (pos limit)
    (goto-char pos)
    (let ((result nil))
      (while (and (null result)
                  (progn
                    (skip-chars-forward "^a-z{}")
                    (< (point) limit)))
        (let ((word-beg (point)))
          (cond
           ((memq (char-after) (list ?{ ?}))
            (forward-char 1))
           (t
            (skip-chars-forward "a-z")))
          (let ((item (assoc (buffer-substring-no-properties word-beg (point)) table)))
            (when item
              (setq result (list (nth 1 item) word-beg (point) (nth 2 item)))))))
      result)))

(defun hl-indent-scope-test--keyword-tree (code next-fn &optional end args)
  "Return the tree for CODE, with block commands found by NEXT-FN.
Optional END limits the range, otherwise it's the whole buffer.
Optional ARGS is passed to `hl-indent-scope-lib-keyword-tree-fn'."
  (with-temp-buffer
    (insert code)
    (save-excursion
      (funcall (apply #'hl-indent-scope-lib-keyword-tree-fn next-fn args)
               (point-min)
               (or end (point-max))))))

(ert-deftest keyword-clause-takes-the-kind-of-its-block ()
  "A clause is ended by the close of its block, whatever kind it is.
Otherwise a clause within a brace block runs past its brace,
adopting the block after it."
  (let ((tree
         (hl-indent-scope-test--keyword-tree
          "{\ncase\na\n}\n{\nb\n}\n"
          (hl-indent-scope-test--keyword-next-fn
           (list (list "{" 'open 'brace) (list "}" 'close 'brace) (list "case" 'clause nil))))))
    ;; Two blocks, last to first, the first containing the clause,
    ;; which ends before the brace at 10.
    (should (equal '(((13 . 16)) ((2 . 10) ((7 . 10)))) tree))))

(ert-deftest keyword-close-ends-a-block-left-open ()
  "A close of a block around the open block ends both.
Otherwise the open block takes it as a stray close, leaving the block
around it open to the end of the buffer, adopting the blocks after it."
  (let ((next-fn
         (hl-indent-scope-test--keyword-next-fn
          (list
           (list "{" 'open 'brace)
           (list "}" 'close 'brace)
           (list "open" 'open nil)
           (list "close" 'close nil)
           (list "case" 'clause nil)))))
    ;; The `open' ends at the brace at 10.
    (should
     (equal
      '(((13 . 16)) ((2 . 10) ((7 . 10))))
      (hl-indent-scope-test--keyword-tree "{\nopen\na\n}\n{\nb\n}\n" next-fn)))
    ;; Through any number of blocks left open, or a clause, ending each at the brace at 15.
    (dolist (code
             (list
              "{\nopen\nopen\na\n}\nopen\nb\nclose\n" "{\nopen\ncase\na\n}\nopen\nb\nclose\n"))
      (should
       (equal
        '(((21 . 24)) ((2 . 15) ((7 . 15) ((12 . 15)))))
        (hl-indent-scope-test--keyword-tree code next-fn))))
    ;; A close of no block around it is ignored.
    (should
     (equal '(((5 . 10))) (hl-indent-scope-test--keyword-tree "open\n}\na\nclose\n" next-fn)))))

(ert-deftest keyword-isolated-kinds ()
  "A close in the body of an isolated block never ends the blocks around it.
Its own close still ends the blocks left open in it."
  (let ((next-fn
         (hl-indent-scope-test--keyword-next-fn
          (list
           (list "open" 'open nil)
           (list "close" 'close nil)
           (list "def" 'open 'define)
           (list "enddef" 'close 'define))))
        (code "open\ndef\nclose\nenddef\nb\nclose\n"))
    ;; The first `close' is in the `def', so the `open' ends at the last.
    (should
     (equal
      '(((5 . 25) ((9 . 16))))
      (hl-indent-scope-test--keyword-tree code next-fn nil (list :isolated-kinds (list 'define)))))
    ;; Unless the kind isn't isolated.
    (should (equal '(((5 . 10) ((9 . 10)))) (hl-indent-scope-test--keyword-tree code next-fn)))
    (should
     (equal
      '(((4 . 10) ((9 . 10))))
      (hl-indent-scope-test--keyword-tree
       "def\nopen\nenddef\n" next-fn nil (list :isolated-kinds (list 'define)))))))

(ert-deftest keyword-tree-fn-arguments ()
  "An unknown key or a key without a value is an error, not silently ignored."
  (dolist (args-and-message
           (list
            (cons (list :isolated-kinds) "missing value")
            (cons (list :isolated (list 'define)) "unknown key")))
    (let ((err
           (should-error
            (apply #'hl-indent-scope-lib-keyword-tree-fn #'ignore (car args-and-message)))))
      (should (string-match-p (cdr args-and-message) (error-message-string err))))))

(ert-deftest keyword-scan-stops-after-the-range ()
  "A block beginning after the range isn't read.
It would only be removed as out of range, when it may be most of the buffer."
  (let ((code "open\na\nclose\nopen\nopen\nb\nclose\nclose\n")
        (commands nil))
    (should
     (equal
      '(((5 . 8)))
      (hl-indent-scope-test--keyword-tree
       code
       (let ((next-fn
              (hl-indent-scope-test--keyword-next-fn
               (list (list "open" 'open nil) (list "close" 'close nil)))))
         (lambda (pos limit)
           (let ((command (funcall next-fn pos limit)))
             (push command commands)
             command)))
       ;; After the first block, within the `open' after it, so its body is out of range.
       15)))
    ;; The first block, then the `open' after it, which ends the scan.
    (should (equal 3 (length commands)))))

(ert-deftest keyword-next-fn-rules ()
  "A function breaking the rules of `hl-indent-scope-lib-keyword-tree-fn' is an error.
Otherwise one that doesn't advance would never return.
Note that the message is checked, as recursing without end is an error too."
  (dolist (command-and-message
           (list
            ;; Empty.
            (cons (list 'open 1 1 nil) "must begin at or after")
            ;; Before the position it was called with, the second call is at 3.
            (cons (list 'open 1 3 nil) "must begin at or after")
            ;; After the limit, the end of the buffer.
            (cons (list 'open 9 10 nil) "must begin at or after")
            (cons (list 'opn 1 3 nil) "invalid command")
            ;; Without a kind.
            (cons (list 'open 1 3) "invalid command")))
    (let ((err
           (should-error
            (hl-indent-scope-test--keyword-tree
             "open\n" (lambda (_pos _limit) (car command-and-message))))))
      (should (string-match-p (cdr command-and-message) (error-message-string err))))))

(provide 'hl-indent-scope-test)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; hl-indent-scope-test.el ends here
