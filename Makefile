# SPDX-License-Identifier: GPL-2.0-or-later

test:
	@emacs -batch -l tests/hl-indent-scope-test.el -f ert-run-tests-batch-and-exit
