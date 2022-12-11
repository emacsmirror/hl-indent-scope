;;; hl-indent-scope-test.el --- Highlight indent scope test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-hl-indent-scope
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

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

(defun hl-indent-scope-test--do-test-on-current-buffer (char-odd char-even)
  "Run a test on the current buffer using CHAR-ODD & CHAR-EVEN."

  (hl-indent-scope-test--clean-buffer char-odd char-even)

  ;; Enable the mode so presets are loaded.
  (hl-indent-scope-mode)

  (hl-indent-scope-buffer)

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
       "if True:\n"
       "\n"
       "@@@@# This is an indented comment.\n"
       "\n"
       "@@@@print('This is true')\n")

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

(provide 'hl-indent-scope-test)
;;; hl-indent-scope-test.el ends here
