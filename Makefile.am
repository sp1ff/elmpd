dist_lisp_LISP = elmpd.el
SUBDIRS = test

dist-hook:
	emacs --batch -q --eval '(checkdoc-file "$(srcdir)/elmpd.el")'
