######################
Highlight Indent Scope
######################

This package highlights indentation based on scope (defined by the syntax-table).

C & C-like language syntax is well supported as well as Lisp's.
Support is also included for CMake and Python.

.. This is a PNG image.

.. figure:: https://codeberg.org/attachments/b50b1aeb-b653-4ca0-9a27-2fcc311d9bab
   :scale: 50 %
   :align: center
   :target: https://codeberg.org/attachments/b50b1aeb-b653-4ca0-9a27-2fcc311d9bab

Available via `melpa <https://melpa.org/#/hl-indent-scope>`__.


Motivation
==========

To provide indentation highlighting that works well for C/C++ projects,
supporting indentation highlighting that is compatible un-indented pre-processor usage.

Further, using scope as opposed to reading different indentation levels has some advantage
in that scope can be differentiated from wrapped function calls (for example),
although this is somewhat a personal preference too.


Usage
=====

Once ``hl-indent-scope-mode`` is enabled, the buffer will highlight indentation based on scope.


Supported Languages
-------------------

``c-mode``
   Supported via a preset which highlights indentation within ``{..}``.

   - ``extern ".." {..}`` blocks are excluded from indentation.

``c++-mode``
   Supported via a preset which highlights indentation within ``{..}``.

   - ``extern ".." {..}`` blocks are excluded from indentation.
   - ``namespace .. {..}`` blocks are excluded from indentation.

``cmake-mode``
   A custom-preset has been included to support CMake.

``python-mode``
   A custom-preset has been included to support Python.

Other C Style Syntax
   Languages such as GLSL/Java/Rust work without depending on presets.

Other Lisp Style Syntax
   Emacs-Lisp has been tested to work with this package without needing a preset.


Customization
-------------

``hl-indent-scope-idle-delay``: 0.2175
   Idle time to wait before highlighting (zero for immediate highlighting).

``hl-indent-scope-fixed-width``: nil
   When non-nil, always use the ``tab-width`` for each indentation level.

``hl-indent-scope-fill-empty-lines``: nil
   Display color columns for blank lines.

``hl-indent-scope-fill-over-text``
   Display colors columns over non white-space characters.

``hl-indent-scope-preset``: t
   Use the default preset for the major modes (when available).

   Otherwise you must configure ``hl-indent-scope-show-block-fn`` yourself.


Callbacks
^^^^^^^^^

When ``hl-indent-scope-preset`` is non-nil the callbacks will be set to provide defaults where possible.

Otherwise you may wish to override these functions to change behavior.

``hl-indent-scope-show-block-fn``: nil
   Function that returns non-nil when the S-expression should be used.

   Takes one ``level`` argument which represents the S-expression depth,
   taking only used levels into account.

   The current state should not be modified.

   When ``nil`` all opening/closing brackets will be used to show indentation levels.

   .. note::

      The ``(point)`` will be located at the start of the S-expression.
      Typically ``(char-before (point))`` can be used to check the kind of bracket.

      An example for C-family languages that only shows indentation for curly braces ``{...}``:

      .. code-block::

         (setq hl-indent-scope-show-block-fn
           (lambda (_level) (eq (char-before (point)) ?{)))


``hl-indent-scope-indent-block-fn``: nil
   Function that returns the indentation level of a block.

   Takes one ``level`` argument which represents the S-expression depth,
   taking only used levels into account.

   The point may be moved, otherwise the current state should not be modified.

   When ``nil`` all blocks use the same indentation level.

   .. note::

      This callback typically doesn't need to be set for any language with C-like syntax.

      Lisps and it's variants are more likely to use different levels of indentation for nested
      blocks, the following example shows how this can be set.

      The ``(point)`` will be located at the start of the S-expression.
      Typically ``(char-before (point))`` can be used to check the kind of bracket.

      An example for Lisp family languages shows how each block can have a different level of indentation:

      .. code-block::

         (setq hl-indent-scope-indent-block-fn
           (lambda (_level) (max 0 (1- (- (point) (line-beginning-position))))))

Faces
-----

The background color for these faces if the background is not already customized or set by the theme.

``hl-indent-scope-odd-face``
   This faces background should be set to the color of odd indentation columns.

``hl-indent-scope-even-face``
   This faces background should be set to the color of even indentation columns.


Installation
============

The package is available in melpa as ``hl-indent-scope``, here is an example with ``use-package``:

.. code-block:: elisp

   (use-package hl-indent-scope
     :commands (hl-indent-scope-mode)
     :hook ((c-mode c++-mode cmake-mode python-mode emacs-lisp-mode) . hl-indent-scope-mode))


Further Work
============

Support Other Languages/Syntax
   Languages that don't use brackets to define scope need custom presets written,
   there is currently no support for languages such as Pascal, Haskell, Ruby and markup such as YAML, XML & HTML.
Tab Support
   This isn't an inherent limitation with the method used, tab support has just not been added.
Other Display Styles
   Other display modes besides odd/even colors could be supported.
